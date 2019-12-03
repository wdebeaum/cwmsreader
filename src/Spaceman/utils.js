const fs = require('fs');
const child_process = require('child_process');
const errors = require('util/cwc/errors.js');

// the kinds of response to get-geographic-region (first two in
// reply-report-answer-:location, the rest are failures):

function fileKQML(name, format) {
  return { 0: 'file', name: `"${name}"`, format: format };
}

function codeKQML(code, standard) {
  return { 0: 'code', code: code.toUpperCase(), standard: `"${standard}"` };
}

// other useful functions

/* modify the array dst by adding all elements of src to the end */
function nconc(dst, src) {
  for (var i = 0, j = dst.length; i < src.length; i++, j++) {
    dst[j] = src[i];
  }
  return dst;
}

/* If a file named name exists, just call the callback with fileKQML; if not,
 * run the program with the given args in order to make the file first.
 */
function makeOrGetFile(name, format, callback, program, ...args) {
  fs.access(name, fs.constants.F_OK, (err)=>{
    if (err) { // does not exist yet, call program
      console.log('making ' + name);
      console.log([program, ...args].join(' '));
      child_process.execFile(program, args, (error, stdout, stderr) => {
	if (error) {
	  callback(errors.programError(error.message + 'stderr output: ' + stderr));
	} else {
	  fs.access(name, fs.constants.F_OK, (err2)=>{
	    if (err2) {
	      callback(errors.programError(`${program} succeeded, but did not create its output file.\nstderr output: ${stderr}`));
	    } else {
	      callback(fileKQML(name, format));
	    }
	  });
	}
      });
    } else { // exists already, just call callback
      callback(fileKQML(name, format));
    }
  });
}

/* Are GeoJSON linear ring coordinate lists a and b equivalent? */
/* unused
function ringsEqual(a, b) {
  if (a.length != b.length) { return false; }
  // decrement length because GeoJSON linear rings repeat the first point as
  // the last point, and we need to ignore that when rotating
  var len = a.length - 1;
  // try to find the rotation of b that makes it the same as a elementwise
  for (var r = 0; r < len; r++) { // rotation of b relative to a
    var found = true;
    for (var i = 0; i < len; i++) { // a index
      var ae = a[i];
      var be = b[(i + r) % len];
      if (!(ae[0] == be[0] && ae[1] == be[1])) {
	found = false;
	break; // next rotation
      }
    }
    if (found) {
      return true;
    }
  }
  return false;
}*/

module.exports = {
  fileKQML: fileKQML,
  codeKQML: codeKQML,
  nconc: nconc,
  makeOrGetFile: makeOrGetFile
};
