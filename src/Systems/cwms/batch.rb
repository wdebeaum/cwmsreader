#!/usr/bin/env ruby

# batch.rb - process a big batch of short (sentence- or paragraph-length) text units in parallel
# 2018-11-08
# William de Beaumont
#
# Run "./batch.rb --help" for usage information.

require_relative '../core/batch.rb'

Options.port_base = 6270
Options.trips_argv = %w{-nouser -reader}
Options.send_to = :reader
#Options.web_parser_name = 'cwmsreader' # not used because we send to reader
Options.archive_files = '*.ekb'

main()

