// maps.js - get human-friendly map images from OSM

const fs = require('fs');
const https = require('https');

const KQML = require('KQML/kqml.js');
const errors = require('util/cwc/errors.js');
const RGBBytes = require('util/cwc/rgb-bytes.js');

const utils = require('utils.js')
const fileKQML = utils.fileKQML;
const makeOrGetFile = utils.makeOrGetFile;

const cacheDir = process.env.TRIPS_BASE + '/etc/Spaceman/cache/osm';

const osmTileFormat = ['raster', '"PNG"', 256, 256];

/* Get the raw OSM tile at the given zoom level and x,y coordinates, and call
 * the callback with its file KQML structure. Each zoom level is a square
 * Mercator projection of the whole world, divided into 2^zoom tiles on a side.
 * Each tile is a 256x256 pixel PNG file. The x and y parameters index the
 * tiles, not the pixels or lat/lon degrees, so they range from 0 to 2^zoom-1.
 * Zoom levels range from 0 to 19.
 */
function getOSMTile(zoom, x, y, callback) {
  //console.log(`getOSMTile(${zoom}, ${x}, ${y}, cb)`);
  const host = 'a.tile.openstreetmap.org'; // TODO? randomly select from a,b,c
  const path = '/' + [zoom, x, y].join('/') + '.png';
  const cacheFile = cacheDir + path;
  //console.log(cacheFile);
  const cacheFileKQML = fileKQML(cacheFile, osmTileFormat);
  fs.access(cacheFile, fs.constants.F_OK, err => {
    if (err) { // does not exist yet, fetch it
      //console.log("...doesn't exist yet")
      console.log('https://' + host + path);
      const req = https.request({
	host: host,
	path: path,
	headers: {
	  'User-Agent': 'TRIPS Spaceman (wbeaumont@ihmc.us)'
	}
      }, res => {
	try {
	  if (res.statusCode != 200) {
	    req.abort();
	    throw errors.programError('tile returned status code ' + res.statusCode + '; headers: ' + JSON.stringify(res.headers));
	  }
	  fs.mkdir(cacheDir + '/' + zoom + '/' + x, { recursive: true }, err=>{
	    try {
	      if (err) throw err;
	      f = fs.createWriteStream(cacheFile)
	      f.on('finish', () => {
		callback(cacheFileKQML);
	      });
	      res.pipe(f);
	    } catch (err) {
	      callback(errors.nestedError('', err));
	    }
	  });
	} catch (err) {
	  callback(errors.nestedError('', err));
	}
      });
      req.end();
    } else { // cacheFile exists already, just call callback
      //console.log('...already exists');
      callback(cacheFileKQML);
    }
  });
  //console.log('returning from getOSMTile');
}

/* Get an image composed of the tiles in the given rectangular region of tile
 * coordinates at the given zoom level, and pass its fileKQML to the callback.
 */
function getOSMTiles(zoom, minX, minY, maxX, maxY, callback) {
  if (minX == maxX && minY == maxY) {
    // special case for exactly one tile
    getOSMTile(zoom, minX, minY, callback);
    return;
  }
  const cacheFile =
    // FIXME? maybe this should be cacheDir/mercator-[box...]-wxh.png, so we can skip crop/zoom when it's not needed?
    cacheDir + '/' + zoom + '/[' +
    ['box', minX, minY, maxX, maxY].join('--') +
    '].png';
  const tileWidth = (maxX - minX + 1);
  const tileHeight = (maxY - minY + 1);
  const numTiles = tileWidth * tileHeight;
  const format = ['vector', '"PNG"', tileWidth * 256, tileHeight * 256];
  // call getOSMTile for each tile, and collect them in raster order in
  // tileFiles (no matter what order they actually complete in); then montage
  // all the files and call the original callback
  const tileFiles = new Array(numTiles);
  const failures = [];
  var numTilesLeft = numTiles;
  function tileDone(i, tileFileKQML) {
    //console.log('tile ' + i + ' done:');
    //console.log(tileFileKQML);
    if (tileFileKQML[0] == 'failure') {
      failures.push(tileFileKQML);
    } else {
      tileFiles[i] = KQML.kqmlStringAsJS(tileFileKQML.name);
    }
    numTilesLeft--;
    if (numTilesLeft == 0) { // all tiles done
      if (failures.length == 0) { // ...successfully!
	// stick them all together
	makeOrGetFile(cacheFile, format, callback,
	  'montage',
	    ...tileFiles,
	    '-tile', tileWidth + 'x' + tileHeight,
	    '-geometry', '256x256+0+0', // geometry of a single tile + 0 padding
	    cacheFile
	);
      } else if (failures.length == 1) { // exactly one tile failed
	callback(failures[0]);
      } else { // more than one tile failed
        callback({ 0: 'failure', type: 'cannot-perform', reason: { 0: 'multiple', failures: failures } });
      }
    }
  }
  var i = 0;
  for (var y = minY; y <= maxY; y++) {
    for (var x = minX; x <= maxX; x++) {
      //console.log({ i: i, x: x, y: y });
      getOSMTile(zoom, x, y, tileDone.bind(this, i));
      i++;
    }
  }
}

/* Convert the given northing (degrees latitude north of the equator) to the
 * corresponding Y coordinate of a square Mercator projection of the given
 * height.
 * See the OSM wiki:
 * https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Derivation_of_tile_names
 */
function northingToMercatorY(northing, height) {
  return height * (1 - Math.asinh(Math.tan(northing * Math.PI / 180)) / Math.PI) / 2;
}

/* Inverse of northingToMercatorY. */
function mercatorYToNorthing(mercatorY, height) {
  return Math.atan(Math.sinh(Math.PI * (1 - 2 * mercatorY / height))) * 180 / Math.PI;
}

/* Convert easting/northing coordinates to real tile x,y coordinates at the
 * given zoom level.
 */
function eastingNorthingToTileXY(zoom, easting, northing) {
  const size = 1 << zoom;
  return [
    (easting + 180) * size / 360,
    northingToMercatorY(northing, size)
  ];
}

/* Inverse of eastingNorthingToTileXY. */
function tileXYToEastingNorthing(zoom, x, y) {
  const size = 1 << zoom;
  return [
    x * 360 / size - 180,
    mercatorYToNorthing(y, size)
  ];
}

/* Return val, min, or max, so that the returned value is an approximation of
 * val in the interval [min,max].
 */
function clamp(val, min, max) {
  return (val < min ? min : (val > max ? max : val));
}

/* Get a map image in Mercator projection with the given pixel dimensions, for
 * the given bounding box in easting-northing coordinates, and pass its
 * fileKQML to the callback. This calls getOSMTiles for a potentially larger
 * area that aligns with tile boundaries at a sufficient zoom level, and then
 * crops and resizes the result.
 */
function getMercatorMap(eastingNorthingBBox, pixelDims, callback) {
  const [_, minEasting, minNorthing, maxEasting, maxNorthing] =
    eastingNorthingBBox;
  const [pixelWidth, pixelHeight] = pixelDims;
  const cacheFile = cacheDir + '/mercator-[' + eastingNorthingBBox.join('--') + ']-' + pixelWidth + 'x' + pixelHeight + '.png';
  const format = ['raster', '"PNG"', pixelWidth, pixelHeight];
  const cacheFileKQML = fileKQML(cacheFile, format);
  fs.access(cacheFile, fs.constants.F_OK, err => {
    if (err) { // does not exist yet, make it
      // get width and height of the desired map in 256-pixel tiles
      const tileWidth = Math.ceil(pixelWidth / 256);
      const tileHeight = Math.ceil(pixelHeight / 256);
      // get the proportion of the world map covered by the desired map in each
      // dimension
      const widthProportion = (maxEasting - minEasting) / 360;
      const heightProportion = northingToMercatorY(minNorthing, 1) -
			       northingToMercatorY(maxNorthing, 1);
      // get the minimum width and height of the world map in tiles
      const minWorldTileWidth = tileWidth / widthProportion;
      const minWorldTileHeight = tileHeight / heightProportion;
      // take the maximum of the two
      const minWorldTileSize = Math.max(minWorldTileWidth, minWorldTileHeight);
      // take the lg to get the zoom level
      // using ceil() here avoids pixellation, but makes labels small
      //const zoom = clamp(Math.ceil(Math.log2(minWorldTileSize)), 0, 19);
      // using floor() makes labels bigger, but can lead to pixellation
      const zoom = clamp(Math.floor(Math.log2(minWorldTileSize)), 0, 19);
      // get the top left tile x,y coordinates
      const [minTileX, minTileY] =
        eastingNorthingToTileXY(zoom, minEasting, maxNorthing);
      const [intMinTileX, intMinTileY] = [minTileX, minTileY].map(Math.floor);
      // get the bottom right tile x,y coordinates
      const [maxTileX, maxTileY] =
        eastingNorthingToTileXY(zoom, maxEasting, minNorthing);
      const [intMaxTileX, intMaxTileY] = [maxTileX, maxTileY].map(Math.ceil);
      getOSMTiles(zoom, intMinTileX, intMinTileY, intMaxTileX, intMaxTileY,
		  tilesFileKQML => {
	if (tilesFileKQML[0] == 'failure') {
	  callback(tilesFileKQML);
	  return;
	}
	const tilesFile = KQML.kqmlStringAsJS(tilesFileKQML.name);
	// crop and resize to get the area desired at the size desired
	const cropMinX = Math.floor(256 * (minTileX - intMinTileX));
	const cropMinY = Math.floor(256 * (minTileY - intMinTileY));
	const cropMaxX = Math.ceil(256 * (maxTileX - intMinTileX));
	const cropMaxY = Math.ceil(256 * (maxTileY - intMinTileY));
	const cropWidth  = cropMaxX - cropMinX; // +1? think floor vs. ceil
	const cropHeight = cropMaxY - cropMinY; //     takes care of it
	makeOrGetFile(cacheFile, format, callback,
	  'convert',
	    tilesFile,
	    '-crop', cropWidth + 'x' + cropHeight + '+' +
		     cropMinX + '+' + cropMinY,
	    '+repage', // keep only the cropped area as the "virtual canvas"
	    '-resize', pixelWidth + 'x' + pixelHeight + '!',
	    cacheFile
	);
      });
    } else { // already exists, just call callback
      callback(cacheFileKQML);
    }
  });
}

/* Get a map image in equirectangular projection with the given pixel
 * dimensions, for the given bounding box in easting-northing coordinates, and
 * pass its fileKQML to the callback. This calls getMercatorMap and then
 * transforms the result from a Mercator projection to an equirectangular
 * projection.
 */
function getEquirectangularMap(eastingNorthingBBox, pixelDims, callback) {
  const [_, minEasting, minNorthing, maxEasting, maxNorthing] =
    eastingNorthingBBox;
  const [pixelWidth, pixelHeight] = pixelDims;
  const cacheFile = cacheDir + '/equirect-[' + eastingNorthingBBox.join('--') + ']-' + pixelWidth + 'x' + pixelHeight + '.png';
  const format = ['raster', '"PNG"', pixelWidth, pixelHeight];
  const cacheFileKQML = fileKQML(cacheFile, format);
  // get the pixel dimensions of the hypothetical equirectangular world map at
  // the correct scale to contain the desired map
  const worldPixelWidth = pixelWidth * 360 / (maxEasting - minEasting);
  const worldEquirectPixelHeight = pixelHeight * 180 / (maxNorthing - minNorthing);
  // not actually needed:
  //const pixelAspectRatio = worldPixelWidth / (worldEquirectPixelHeight * 2);
  // get the pixel height of the hypothetical square Mercator world map at the
  // same horizontal scale
  const worldMercatorPixelHeight = worldEquirectPixelHeight * 2;
  //   = worldPixelWidth / pixelAspectRatio;
  // get the pixel height of the Mercator version of the desired map
  const mercatorMinY = northingToMercatorY(maxNorthing, worldMercatorPixelHeight);
  const mercatorMaxY = northingToMercatorY(minNorthing, worldMercatorPixelHeight);
  const mercatorPixelHeight = Math.round(mercatorMaxY - mercatorMinY);
  /*console.log({
    worldPixelWidth:worldPixelWidth,
    worldEquirectPixelHeight:worldEquirectPixelHeight,
    worldMercatorPixelHeight:worldMercatorPixelHeight,
    mercatorMinY:mercatorMinY,
    mercatorMaxY:mercatorMaxY,
    mercatorPixelHeight:mercatorPixelHeight
  });*/
  // get the Mercator version of the desired map
  getMercatorMap(eastingNorthingBBox, [pixelWidth, mercatorPixelHeight],
		 mercatorFileKQML => {
    if (mercatorFileKQML[0] == 'failure') {
      callback(mercatorFileKQML);
      return;
    }
    const mercatorFile = KQML.kqmlStringAsJS(mercatorFileKQML.name);
    // transform the map from Mercator to equirectangular projection
    RGBBytes.readFileWithDims(mercatorFile, pixelWidth, mercatorPixelHeight,
			      mercatorRGB => {
      if (mercatorRGB[0] == 'failure') {
	callback(mercatorRGB);
	return;
      }
      const equirectRGB = new RGBBytes(pixelWidth, pixelHeight);
      // for each pixel of equirectRGB, set it to the average of two samples
      // from mercatorRGB (which might still land on the same pixel)
      for (var ey = 0; ey < pixelHeight; ey++) {
	// translate equirect Y to mercator Y (within the image), for two
	// samples within the same equirect pixel so we can anti-alias
	var ey1 = ey;
	var ey2 = ey + 0.5;
	var my1 =
	  Math.floor(
	    northingToMercatorY(
	      maxNorthing - ey1 * (maxNorthing - minNorthing) / pixelHeight,
	      worldMercatorPixelHeight
	    ) - mercatorMinY
	  );
	var my2 =
	  Math.floor(
	    northingToMercatorY(
	      maxNorthing - ey2 * (maxNorthing - minNorthing) / pixelHeight,
	      worldMercatorPixelHeight
	    ) - mercatorMinY
	  );
	for (var x = 0; x < pixelWidth; x++) {
	  // average the two samples for this pixel
	  var c1 = mercatorRGB.get(x, my1);
	  var c2 = mercatorRGB.get(x, my2);
	  var c = {
	    r: Math.round((c1.r + c2.r)/2),
	    g: Math.round((c1.g + c2.g)/2),
	    b: Math.round((c1.b + c2.b)/2)
	  };
	  equirectRGB.set(x, ey, c);
	}
      }
      equirectRGB.writeFile(cacheFile, callback);
    });
  });
}

module.exports = {
  getEquirectangularMap: getEquirectangularMap
};
