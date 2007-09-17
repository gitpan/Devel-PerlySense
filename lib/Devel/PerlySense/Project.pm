=head1 NAME

Devel::PerlySense::Project - A Project root

=head1 SYNOPSIS




=head1 DESCRIPTION

A Project describes the root directory of a source tree.

A Project has configuration settings.

=cut





package Devel::PerlySense::Project;

our $VERSION = '0.01';





use strict;
use warnings;
use Spiffy -Base;
use Carp;
use Data::Dumper;
use File::Basename;
use Path::Class;
use File::Slurp;

use Devel::PerlySense;





=head1 PROPERTIES

=head2 dirProject

The effective project root dir.

Readonly.

=cut
sub dirProject {
    return
            $self->dirProjectExplicitDir ||
            $self->dirProjectImplicitUse ||
            $self->dirProjectImplicitDir;
}





=head2 dirProjectExplicitDir

If known, the root dir made explicit by the existance of a project
directory and config, else ""

Default: ""

=cut
field "dirProjectExplicitDir" => "";





=head2 dirProjectImplicitUse

If known, the root dir indicated by a found used module, else "".

Default: ""

=cut
field "dirProjectImplicitUse" => "";





=head2 dirProjectImplicitDir

If known, the root dir indicated by the presence of "lib" or "t", else "".

Default: ""

=cut
field "dirProjectImplicitDir" => "";





=head2 oPerlySense

Devel::PerlySense object.

=cut
field "oPerlySense" => undef;





=head1 API METHODS

=head2 new()

Create new Location object.

=cut
# sub new(@) {
#     my $pkg = shift;
#     my (%p) = @_;

#     my $self = bless {}, $pkg;

#     $self->file($p{file} || "");
#     $self->row($p{row} || 0);
#     $self->col($p{col} || 0);
#     $self->rhProperty($p{rhProperty} || {});

#     return($self);    
# }





=head2 newFromLocation(file => $file, dir => $dir, oPerlySense => $oPs)

Create new Project given either $file, or $dir.

First, search for an explicit project root directory, then try to find
any modules used in $file (if passed), then try to find any "lib" or
"t" directory upwards of $file or $dir.

$file takes precedence over $dir if both are specified.

If none if this works out, no Project can be created and undef is
returned.

Return the new object, or undef if no project could be found.

=cut
sub newFromLocation(@) {
    my $pkg = shift;
    my ($oPerlySense) = Devel::PerlySense::Util::aNamedArg(["oPerlySense"], @_);
    my %p = @_;
    my $file = $p{file};
    my $dir = $p{dir};
    $file and $dir = dirname($file);
    $dir and $dir = dir($dir)->absolute;

    #Find explicit project
    if($dir and my $dirProject = $oPerlySense->dirFindLookingAround(
        ".PerlySenseProject",
        $dir,
        ["."],
    )) {
        return $pkg->new(
            oPerlySense => $oPerlySense,
            dirProjectExplicitDir => $dirProject,
        );
    }
    
    #If file, look for the dir from where any package in the file can
    #be used
    if($file and my $oDocument = $oPerlySense->oDocumentParseFile($file)) {
        for my $package ($oDocument->aNamePackage) {
            my $filePackage = $oPerlySense->fileFromModule($package);
            my $dirFound = $oPerlySense->dirFindLookingAround($filePackage, $dir);

            if($dirFound) {
                my $dirProject = dir($dirFound)->parent . ""; ###TODO: if lib or bin
                return $pkg->new(
                    oPerlySense => $oPerlySense,
                    dirProjectImplicitUse => $dirProject,
                );
            }
        }
    }

    #If dir, look for dirs
    if($dir) {
        my $dirFound =
                   $oPerlySense->fileFindLookingAround("lib", $dir) ||
                   $oPerlySense->fileFindLookingAround("t", $dir);
        
        if($dirFound) {
            my $dirProject = dir($dirFound)->parent . ""; ###TODO: if lib or bin
            return $pkg->new(
                oPerlySense => $oPerlySense,
                dirProjectImplicitDir => $dirProject,
            );
        }
    }
    
    return(undef);
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
