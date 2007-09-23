=head1 NAME

Devel::PerlySense::Config::Project - A Project's configuration

=head1 SYNOPSIS




=head1 DESCRIPTION

Configuration can be Project level, stored in .../.PerlySense/project.cfg

=cut





package Devel::PerlySense::Config::Project;

our $VERSION = '0.01';





use strict;
use warnings;

use Spiffy -Base;

use Data::Dumper;
use Carp;

use File::Basename;
use File::Path;
use Path::Class;
use YAML::Tiny ();
use HTTP::Date qw/ time2iso /;

use Devel::PerlySense::Util;





=head1 PROPERTIES

=head2 nameFileConfig

The config file name relative to the root dir.

Default: ./PerlySenseProject/project.cfg

=cut
field "nameFileConfig" => ".PerlySenseProject/project.yml";






=head2 textConfigDefault

The default contents of the config file

=cut
field "textConfigDefault" => q{#PerlySense Project Config

#What's this all about? See: http://search.cpan.org/dist/Devel-PerlySense/

project:

  #The human readable name of the Project
  moniker: 'A PerlySense Project'

  #Extra @INC directories, relative to the project root
  #These come before the default inc directories "." and "lib"
  inc_dir: []


bookmarks:
  -
    moniker: Todo
    rex: ### \s* TODO \s* : \s* (.+?) \s*
    rex_match_text: "$1"


#These are evaluated in order to find a way to run a file. First
#match is used.
run_file:
  -
    command: "prove -v ${INC} \"${SOURCE_FILE}\""
    moniker: Test
    rex: \.t$
    run_from: source_root_directory
  -
    command: "perl -c ${INC} \"${SOURCE_FILE}\" 2>&1| perl -ne \"/Subroutine \\w+ redefined at/ or print\""
    moniker: Module
    rex: \.pm$
    run_from: source_root_directory
  -
    command: "perl ${INC} \"${SOURCE_FILE}\""
    moniker: Script
    rex: \.pl$
    run_from: file_directory

  #This is a catch-all for all other types of files
  -
    command: "perl ${INC} \"${SOURCE_FILE}\""
    moniker: 'Script (no .pl)'
    rex: .
    run_from: file_directory


external:
  editor:

    #Emacs specific configuration
    emacs:

      #To enable flymake in Emacs, add this to your .emacs file, after
      #loading cperl-mode and perly-sense.
      #
      # ;; Flymake
      # (load "perly-sense-flymake")
      #
      # ;; If you only want syntax check whenever you save, not continously
      # (setq flymake-no-changes-timeout 9999)
      # (setq flymake-start-syntax-check-on-newline nil)
      #
      # ;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
      # (set-face-background 'flymake-errline "antique white")
      # (set-face-background 'flymake-warnline "lavender")
      flymake:

        #During a flymake compilation, perly_sense can run:

        #Check Syntax (perl -c)
        #
        #!!!NOTE!!!
        #   Running perl -c on random Perl code will execute
        #   the BEGIN blocks! Any funny business in them and you're toast!
        #!!!NOTE!!!
        syntax: 1


#EOF
};





=head2 dirRoot

The root directory of the loaded config, or undef if no config is
loaded yet.

Default: undef

=cut
field "dirRoot" => undef;





=head2 rhConfig

The hash ref with the currently loaded config.

Default: { }

=cut
field "rhConfig" => { };





=head2 new()

Create new Location object.

=cut
sub new(@) {
    my $pkg = shift;

    my $self = $pkg->SUPER::new(@_);

    $self->dirRoot and $self->loadConfig(dirRoot => $self->dirRoot);

    return($self);
}





=head1 METHODS

=head2 loadConfig(dirRoot => DIR)

Load the file for $dirRoot and set dirDoor and rhConfig.

Return 1 if the file could be loaded, else die, e.g. if the file
contains syntax errors.

=cut
sub loadConfig {
    my ($dirRoot) = Devel::PerlySense::Util::aNamedArg(["dirRoot"], @_);

    my $fileConfig = file($dirRoot, $self->nameFileConfig);
    my $sourceConfig = slurp($fileConfig) or
            die("Could not open config file ($fileConfig)\n");
    my ($rhConfig) = YAML::Tiny::Load($sourceConfig);
    $rhConfig or die($YAML::Tiny::errstr);

    $self->dirRoot($dirRoot);
    $self->rhConfig($rhConfig);

    return 1;
}





=head2 createFileConfigDefault(dirRoot => DIR)

Create a config file under $dirRoot with the default config and load
it. Create directories as needed.

If there is an existing config file, rename it first to end with the
current time.

Return 1, or die on errors.

=cut
sub createFileConfigDefault {
    my ($dirRoot) = Devel::PerlySense::Util::aNamedArg(["dirRoot"], @_);

    my $fileConfig = file($dirRoot, $self->nameFileConfig);
    my $dirConfig = dirname($fileConfig);
    mkpath($dirConfig);
    -d $dirConfig or die("Could not create config directory ($dirConfig)\n");

    if(-e $fileConfig) {
        my $textTime = time2iso( time() );
        $textTime =~ s/\W+/_/g;
        my $fileConfigBackup = $fileConfig . "." . $textTime;

        rename($fileConfig, $fileConfigBackup)
                or die("Could not rename ($fileConfig) -> ($fileConfigBackup)\n");
    }

    spew($fileConfig, $self->textConfigDefault) or
            die("Could not create config file ($fileConfig)\n");

    $self->loadConfig(dirRoot => $dirRoot);

    return 1;
}





1;





__END__

=head1 AUTHOR

Johan Lindström, C<< <johanl[ÄT]DarSerMan.com> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-devel-perlysense@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-PerlySense>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2005 Johan Lindström, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
