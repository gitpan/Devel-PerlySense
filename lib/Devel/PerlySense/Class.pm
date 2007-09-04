=head1 NAME

Devel::PerlySense::Class - A Perl Class

=head1 SYNOPSIS



=head1 DESCRIPTION

A Perl Class is a Perl Package with an OO interface.

=cut





package Devel::PerlySense::Class;

our $VERSION = '0.01';





use strict;
use warnings;
use Spiffy -Base;
use Carp;
use Data::Dumper;
use File::Basename;

use Devel::PerlySense;
use Devel::PerlySense::Util;
use Devel::PerlySense::Document::Location;
use Devel::PerlySense::Document::Api;
use Devel::PerlySense::Document::Meta;

use Devel::TimeThis;





=head1 PROPERTIES

=head2 oPerlySense

Devel::PerlySense object.

Default: set during new()

=cut
field "oPerlySense" => undef;





=head2 name

The Class name (i.e. the package name)

Default: ""

=cut
field "name" => "";





=head2 raDocument

Array ref with PerlySense::Document objects that define this class.

Default: []

=cut
field "raDocument" => [];





=head2 rhClassBase

Hash ref with (keys: base class names; values: base class
PerlySense::Class objects).

Default: {}

=cut
###TODO: Make this lazy, populate on first request, so we don't have
###to go all the way up all the time! There are enough objects in
###memory as it is.
field "rhClassBase" => {};





=head1 API METHODS

=head2 new(oPerlySense, name, raDocument, rhClassSeen => {})

Create new PerlySense::Class object. Give it $name and associate it
with $oPerlySense.

$rhClassSeen is used to keep track of seen base classes in case we
encounter circular deps.

=cut
sub new {
    my ($oPerlySense, $name, $raDocument) = Devel::PerlySense::Util::aNamedArg(["oPerlySense", "name", "raDocument"], @_);
    my $rhClassSeen = {@_}->{rhClassSeen};

    $self = bless {}, $self;    #Create the object. It looks weird because of Spiffy
    $self->oPerlySense($oPerlySense);
    $self->name($name);
    $self->raDocument($raDocument);

    $rhClassSeen ||= { $name => $self };
    $self->findBaseClasses(rhClassSeen => $rhClassSeen);
    
    return($self);
}





=head2 newFromFileAt(oPerlySense => $oPerlySense, file => $file, row => $row, col => $col)

Create new PerlySense::Class object given the class found at $row,
$col in $file.

Return new object, or undef if no class was found, or die if the file
doesn't exist.

=cut
sub newFromFileAt {
    my ($oPerlySense, $file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["oPerlySense", "file", "row", "col"], @_);

    my $oDocument = $oPerlySense->oDocumentParseFile($file);    
    my $package = $oDocument->packageAt(row => $row);
    $package eq "main" and return undef;
    
    my $class = Devel::PerlySense::Class->new(
        oPerlySense => $oPerlySense,
        name => $package,
        raDocument => [ $oDocument ],
    );

    return($class);
}





=head2 newFromName(oPerlySense, name, dirOrigin, rhClassSeen)

Create new PerlySense::Class object given the class $name.

Look for the module file starting at $dirOrigin.

Return new object, or undef if no class was found with that $name.

=cut
sub newFromName {
    my ($oPerlySense, $name, $dirOrigin, $rhClassSeen) = Devel::PerlySense::Util::aNamedArg(["oPerlySense", "name", "dirOrigin", "rhClassSeen"], @_);

    my $oDocument = $oPerlySense->oDocumentFindModule(
        nameModule => $name,
        dirOrigin => $dirOrigin,
    ) or return undef;
    
    my $class = Devel::PerlySense::Class->new(
        rhClassSeen => $rhClassSeen,
        oPerlySense => $oPerlySense,
        name => $name,
        raDocument => [ $oDocument ],
    );

    return($class);
}





=head2 findBaseClasses(rhClassSeen)

Find the base classes of this class and set (replace) rBaseClass with
newly created Class objects.

Reuse any class names and objects in $rhClassSeen (keys: class names;
values: Class objects), i.e. don't follow them upwards, they have
already been taken care of.

=cut
sub findBaseClasses {
    my ($rhClassSeen) = Devel::PerlySense::Util::aNamedArg(["rhClassSeen"], @_);

    my $rhClassBase = {};

    debug("Checking class (" . $self->name . ") for inheritance\n");
    
    for my $oDocument (@{$self->raDocument}) {
        for my $classNameBase ($oDocument->aNameBase) {
            debug("  Base for (" . $self->name . ") is ($classNameBase)\n");
            my $classBase =
                    $rhClassSeen->{$classNameBase} ||
                    ref($self)->newFromName(
                        oPerlySense => $self->oPerlySense,
                        rhClassSeen => $rhClassSeen,
                        name => $classNameBase,
                        dirOrigin => dirname($oDocument->file),
                    ) or warn("Could not find parent ($classNameBase)\n"), next;  #Don't stop if we can't find the base class. Maybe warn?
            
            $rhClassSeen->{$classNameBase} = $classBase;

            $rhClassBase->{$classNameBase} = $classBase;
        }
    }
    
    $self->rhClassBase($rhClassBase);

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
