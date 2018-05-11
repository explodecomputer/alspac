#!/usr/bin/perl

######################################################################
# spssread.pl - a utility to print info about SPSS data files
# version 0.2.1 on 16 Jun 2005 by Scott Czepiel <sczepiel@gmail.com>
# for updates see: <http://czep.net/data/spssread/>
# Usage: ./spssread.pl [OPTION] [spss-filename.sav]
# Choose one of the following single-character options:
#   -h  Print File Header information
#   -r  Print tab-delimited info about Variables
#   -l  Print tab-delimited info about Value Labels
#
#  Copyright (C) 2005 Scott Czepiel <http://czep.net/>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
#  USA
#
######################################################################

######################################################################
# Module Directives
######################################################################
use strict;

######################################################################
# Global Variables
######################################################################
my $savfile;
my $buf;
my $opt;

my $header = {};
my $internals = {};
my @varrecs = ();
my @valrecs = ();
my @valrefs = ();

# SYMBOLIC CONSTANTS
my $SIZEOFINT = 4;
my $SIZEOFDBL = 8;

my $HEADERLEN = 148 + 5 * $SIZEOFINT + $SIZEOFDBL;
my $VARNAMELEN = 8;

my $ERR1 = "Insufficient file header length";
my $ERR2 = "Unexpected termination of variable records at record number ";
my $ERR3 = "Unexpected termination of dictionary header";
my $ERR4 = "Invalid value label record";
my $ERR5 = "Invalid document record";
my $ERR6 = "Invalid miscellaneous record";


######################################################################
# Main Application Logic
######################################################################

# parse the command line
# require one option and one input file
if ($#ARGV != 1) {
    &print_help();
}

# allow a single option optionally prefaced with a '-'
if (@ARGV[0] =~ /^\-*([hrl]{1})/i) {
    $opt = $1;
}
else {
    &print_help();
}

# open the spss sav file named as the second arg for read in binary mode
$savfile = @ARGV[1];
open(SAV, '<', $savfile) or die "Unable to open file $savfile:\n$!\n";
binmode(SAV);

# read the entire dictionary before doing any printing
&read_file_header();
&read_variable_records();
&read_remaining_dict();
close(SAV);

# process printing option
if ($opt eq 'h') {
    &print_file_header();
}
elsif ($opt eq 'r') {
    &print_variables();
}
elsif ($opt eq 'l') {
    &print_value_labels();
}
else {
    &print_help();
}

exit(0);


######################################################################
#
# END OF APPLICATION LOGIC, ALL SUBROUTINES ARE BELOW
#
######################################################################


##################################################
# sub read_file_header
# Read and parse the SAV file header
##################################################
sub read_file_header {

    # read the file header into buffer
    if (sysread(SAV, $buf, $HEADERLEN) != $HEADERLEN) { err($ERR1); }

    # set the buffer offset to 0
    my $bos = 0;

    # record type code, 4 byte character string, should be "$FL2"
    $header->{RECTYPE} = substr($buf, $bos, 4);
    $bos += 4;

    # abort if we don't see the magic string here
    if ($header->{RECTYPE} ne '$FL2') {
        err("This file does not appear to be an SPSS SAV file.");
    }

    # product name, 60 byte character string
    # typical value: "@(#) SPSS DATA FILE MS WINDOWS Release 8.0 SPSSIO32.DLL"
    $header->{PRODNAME} = substr($buf, $bos, 60);
    $bos += 60;

    # file layout code, integer
    $header->{LAYOUTCODE} = unpack('i', substr($buf, $bos, $SIZEOFINT));
    $bos += $SIZEOFINT;

    # number of OBS elements per case, integer
    $header->{CASESIZE} = unpack('i', substr($buf, $bos, $SIZEOFINT));
    $bos += $SIZEOFINT;

    # compression switch, integer
    $header->{COMPRESS} = unpack('i', substr($buf, $bos, $SIZEOFINT));
    $bos += $SIZEOFINT;

    # index of weight variable, integer
    $header->{WGTINDEX} = unpack('i', substr($buf, $bos, $SIZEOFINT));
    $bos += $SIZEOFINT;

    # number of cases, integer
    $header->{NUMCASES} = unpack('i', substr($buf, $bos, $SIZEOFINT));
    $bos += $SIZEOFINT;

    # compression bias, double-precision floating point
    $header->{BIAS} = unpack('d', substr($buf, $bos, $SIZEOFDBL));
    $bos += $SIZEOFDBL;

    # creation date, 9 byte character string
    $header->{CRDATE} = substr($buf, $bos, 9);
    $bos += 9;

    # creation time, 8 byte character string
    $header->{CRTIME} = substr($buf, $bos, 8);
    $bos += 8;

    # file label, 64 byte character string
    $header->{FILELABEL} = substr($buf, $bos, 64);

}


##################################################
# sub print_file_header
##################################################
sub print_file_header {

    # print the file header
    print "\n";
    printf("%-20s%s\n", "Record type",     $header->{RECTYPE});
    printf("%-20s%s\n", "Product name",    $header->{PRODNAME});
    printf("%-20s%d\n", "Layout code",     $header->{LAYOUTCODE});
    printf("%-20s%d\n", "Case Size",       $header->{CASESIZE});
    printf("%-20s%d\n", "Compression",     $header->{COMPRESS});
    printf("%-20s%d\n", "Weight index",    $header->{WGTINDEX});
    printf("%-20s%d\n", "Number of cases", $header->{NUMCASES});
    printf("%-20s%.6f\n", "Bias",          $header->{BIAS});
    printf("%-20s%s\n", "Creation date",   $header->{CRDATE});
    printf("%-20s%s\n", "Creation time",   $header->{CRTIME});
    printf("%-20s%s\n", "File label",      $header->{FILELABEL});

}


##################################################
# sub read_variable_records
# Read and parse variable records
##################################################
sub read_variable_records {

    # main loop for each OBS record
    for (my $i = 0; $i < $header->{CASESIZE}; $i++) {

        my $rec = {};

        # read next record type code, err if not type 2
        if ((sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) or unpack('i', $buf) != 2) { err("$ERR2$i"); }

        # read variable type code
        if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR2$i"); }
        $rec->{TYPECODE} = unpack('i', $buf);

        # if type is not -1, then record is for a numeric var or the
        # first (and only) instance of a string var
        if ($rec->{TYPECODE} != -1) {

            # read label flag
            if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR2$i"); }
            $rec->{HASLABEL} = unpack('i', $buf);

            # read missing value format code
            if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR2$i"); }
            $rec->{NMISSING} = unpack('i', $buf);

            # read print format code
            if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR2$i"); }
            $rec->{PRINTFMT} = unpack('i', $buf);

            # read write format code
            if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR2$i"); }
            $rec->{WRITEFMT} = unpack('i', $buf);

            # read varname
            if (sysread(SAV, $buf, $VARNAMELEN) != $VARNAMELEN) { err("$ERR2$i"); }
            $rec->{VARNAME} = $buf;

            # read label length and label only if a label exists
            if ($rec->{HASLABEL} == 1) {

                # read label length
                if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR2$i"); }
                $rec->{LABELLEN} = unpack('i', $buf);

                # round label len up to nearest multiple of 4 bytes
                my $tmp_len = $rec->{LABELLEN};
                my $tmp_mod = ($rec->{LABELLEN} % 4);
                if ($tmp_mod != 0) {
                    $tmp_len += 4 - $tmp_mod;
                }

                # read label
                if (sysread(SAV, $buf, $tmp_len) != $tmp_len) { err("$ERR2$i"); }
                $rec->{VARLABEL} = $buf;

            }

            # read missing values only if present
            if ($rec->{NMISSING} != 0) {

                # read each missing value, but keep them packed
                # since they are raw data items (doubles)
                my $misslen = $SIZEOFDBL * abs($rec->{NMISSING});
                if (sysread(SAV, $buf, $misslen) != $misslen) { err("$ERR2$i"); }
                $rec->{MISSINGS} = $buf;

            }

        }   # end if TYPECODE is not -1

        # if TYPECODE is -1, record is a continuation of a string var
        else {

            # read and ignore the next 24 bytes
            if (sysread(SAV, $buf, 24) != 24) { err("$ERR2$i"); }

        }

        # add to the array
        push @varrecs, $rec;

        #print "$i\t@varrecs[$i]->{VARNAME}\n";

    }   # loop to next variable record

}


##################################################
# sub print_variables
##################################################
sub print_variables {

    my $outstr = "";

    # print header
    print "name\ttype\tlabel\n";

    # loop for each OBS record
    for (my $i = 0; $i < $header->{CASESIZE}; $i++) {

        # only continue if TYPECODE is not -1
        # -1 means a continuation of a prior string var
        if (@varrecs[$i]->{TYPECODE} != -1) {

            # build output string - name
            $outstr = "@varrecs[$i]->{VARNAME}\t";

            # append type
            if (@varrecs[$i]->{TYPECODE} == 0) {
                $outstr .= "Numeric\t";
            }
            else {
                $outstr .= "String (" . @varrecs[$i]->{TYPECODE} . ")\t";
            }

            # append label if exists
            if (@varrecs[$i]->{HASLABEL} == 1) {
                $outstr .= "@varrecs[$i]->{VARLABEL}\n";
            }
            else {
                $outstr .= " \n";
            }

            # print
            print "$outstr";
        }
    }

}


##################################################
# sub read_remaining_dict
# Read all other records in the dictionary
##################################################
sub read_remaining_dict {

    my $end_of_header = 0;

    while (!$end_of_header) {

        # read the next record type
        if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR3"); }
        my $rectype = unpack('i', $buf);

        # handle this record type
        if ($rectype == 3) {
            &read_value_labels();
        }
        elsif ($rectype == 6) {
            &read_document_record();
        }
        elsif ($rectype == 7) {
            &read_misc_record();
        }
        elsif ($rectype == 999) {

            # dictionary termination record

            # save the number of value label records
            $internals->{NUMVALUELABELS} = $#valrecs;

            # read past one int of filler
            if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR3"); }
            $end_of_header = 1;
        }
        else {
            err("Invalid Record type found in dictionary: $rectype");
        }

    }
}


##################################################
# sub read_value_labels
# Read and parse value label records
##################################################
sub read_value_labels {

    my $rec = {};
    my $refrec = {};
    my @vals = ();
    my @labs = ();
    my @refs = ();
    my $tmp_len = 0;
    my $tmp_mod = 0;

    # read the count of value/label pairs
    if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { warn("foo"); err("$ERR4"); }
    $rec->{COUNT} = unpack('i', $buf);

    # do for each pair
    for (my $i = 0; $i < $rec->{COUNT}; $i++) {

        # read the next value
        if (sysread(SAV, $buf, $SIZEOFDBL) != $SIZEOFDBL) { warn("foo"); err("$ERR4"); }
        push @vals, $buf;

        # read the next byte to get the label length
        if (sysread(SAV, $buf, 1) != 1) { warn("foo"); err("$ERR4"); }
        $tmp_len = unpack('c', $buf);
        if ($tmp_len > 255) { warn("foo"); err("$ERR4"); }

        # round label len up to nearest multiple of 8 bytes
        $tmp_mod = (($tmp_len + 1) % 8);
        if ($tmp_mod != 0) {
            $tmp_len += 8 - $tmp_mod;
        }

        # read the label
        if (sysread(SAV, $buf, $tmp_len) != $tmp_len) { warn("foo"); err("$ERR4"); }
        push @labs, $buf;

    }

    # store the valrecs record
    $rec->{VALUES} = [ @vals ];
    $rec->{LABELS} = [ @labs ];
    push @valrecs, $rec;

    # read the corresponding variable reference record

    # record type must be 4
    if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { warn("foo"); err("$ERR4"); }
    if (unpack('i', $buf) != 4) { warn("foo"); err("$ERR4"); }

    # read count of vars that are assigned to this val record
    if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { warn("foo"); err("$ERR4"); }
    $refrec->{COUNT} = unpack('i', $buf);

    # do for each variable index
    for (my $i = 0; $i < $refrec->{COUNT}; $i++) {

        if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { warn("foo"); err("$ERR4"); }
        push @refs, unpack('i', $buf);

    }

    # store the valrefs record
    $refrec->{VARS} = [ @refs ];
    push @valrefs, $refrec;

}


##################################################
# sub print_value_labels
##################################################
sub print_value_labels {

    my $outstr = "";
    my $varnum = 0;
    my $varname = "";
    my $val = 0;
    my $lab = "";

    # print header
    print "Varname\tValue\tLabel\n";

    # loop for each valrec
    for my $i ( 0 .. $#valrecs ) {

        # loop for each var referenced by this valrec
        for (my $j = 0; $j < @valrefs[$i]->{COUNT}; $j++) {

            $varnum = @valrefs[$i]->{VARS}[$j] - 1;
            $varname = @varrecs[$varnum]->{VARNAME};

            # loop for each value/label pair
            for (my $k = 0; $k < @valrecs[$i]->{COUNT}; $k++) {

                $val = unpack('d', @valrecs[$i]->{VALUES}[$k]);
                $lab = @valrecs[$i]->{LABELS}[$k];
                $outstr = sprintf("%s\t%.0f\t%s\n", $varname, $val, $lab);
                print "$outstr";

            }

        }

    }

}


##################################################
# sub read_document_record
# Read and parse document records
##################################################
sub read_document_record {

    my $count = 0;
    my $text = "";

    # read count
    if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR5"); }
    $count = unpack('i', $buf);

    # read but ignore documentary info
    for (my $i = 0; $i < $count; $i++) {
        if (sysread(SAV, $buf, 80) != 80) { err("$ERR5"); }
    }

}


##################################################
# sub read_misc_record
# Read and parse miscellaneous records
##################################################
sub read_misc_record {

    my $subtype = 0;
    my $datalen = 0;
    my $count = 0;

    # read subtype
    if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR6"); }
    $subtype = unpack('i', $buf);

    # read datalen
    if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR6"); }
    $datalen = unpack('i', $buf);

    # read count
    if (sysread(SAV, $buf, $SIZEOFINT) != $SIZEOFINT) { err("$ERR6"); }
    $count = unpack('i', $buf);

    # if this is a flt64 record, read internals
    if ($subtype == 4 and $datalen == 8 and $count == 3) {

        if (sysread(SAV, $internals->{SYSMIS}, $SIZEOFDBL) != $SIZEOFDBL) { err("$ERR6"); }
        if (sysread(SAV, $internals->{HI}, $SIZEOFDBL) != $SIZEOFDBL) { err("$ERR6"); }
        if (sysread(SAV, $internals->{LO}, $SIZEOFDBL) != $SIZEOFDBL) { err("$ERR6"); }
    }

    else {

        # read but ignore generic data
        my $nbytes = $datalen * $count;
        if (sysread(SAV, $buf, $nbytes) != $nbytes) { err("$ERR6"); }
    }

}


##################################################
# sub err
# Report an error reading SAV file and exit
##################################################
sub err {
    my $errtxt = shift(@_);
    print "Error reading SAV file: $errtxt\n";
    close(SAV);
    exit(0);
}


##################################################
# sub print_help
# Print usage info and exit
##################################################
sub print_help {

    printf("\n%s\n%s\n%s\n\n%s\n%s\n%s\n%s\n%s\n\n",
        'spssread.pl - a utility to print info about SPSS data files',
        'version 0.2.1 on 16 Jun 2005 by Scott Czepiel <sczepiel@gmail.com>',
        'for updates see: <http://czep.net/data/spssread/>',
        'Usage: ./spssread.pl [OPTION] [spss-filename.sav]',
        'Choose one of the following single-character options:',
        '  -h  Print File Header information',
        '  -r  Print tab-delimited info about Variables',
        '  -l  Print tab-delimited info about Value Labels',
    ); 

    exit(0);
}


######################################################################
# END OF FILE: spssread.pl
######################################################################