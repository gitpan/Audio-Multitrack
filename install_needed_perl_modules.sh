#!/bin/sh
# 
# Run this script to obtain required modules if you 
# are not installing this module via CPAN
#
cpan -i IO::All Carp Cwd Tk Storable Getopt::Std Audio::Ecasound Parse::RecDescent Term::ReadLine Data::YAML File::Find::Rule File::Spec::Link
