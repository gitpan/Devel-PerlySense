=head1 NAME

Devel::PerlySense::Plugin::Syntax::Moose - Plugin for parsing Moose syntax
constructs

=head1 DESCRIPTION

Parses Moose specific syntax, e.g. "extends", etc.

Currently supported:


=over 4

=item * extends - Single and multiple inheritance

=item * with - Role composition, is treated as a base class

=back



=head1 KNOWN MOOSE BUGS

Broken Moose code, e.g. multiple extends are parsed incorrectly (the
ISA isn't reset). But you shouldn't have broken Moose code should you?

Roles are treated like base classes, because that's the most similar
Perl concept.



=head1 KNOWN BUGS

This plugin module is not yet it's own distribution, which it should
be. It should have a base class inside the PerlySense distro.

The plugins could have some kind of marker for when they should be run
for a document. It could be a quick regex on the source or per line or
something.

=cut





use strict;
use warnings;

package Devel::PerlySense::Plugin::Syntax::Moose;

our $VERSION = '0.01';





use Spiffy -Base;
use Carp;
use Data::Dumper;
use PPI::Document;
use PPI::Dumper;





=head1 PROPERTIES

=head1 API METHODS

=cut





=head2 parse($rhDataDocument, $oMeta, $oDocument, $oNode, $pkgNode, $row, $col, $packageCurrent)

Parse the Devel::PerlySense::Document and extract metadata. Fill
appropriate data structures.

rhDataDocument

the key e.g. "Moose" for Plugin::Syntax::Moose, is for the plugin to
manage. It's persistent during the complete parse of a document.

Return 1 or die on errors.

=cut
sub parse {
    my ($rhDataDocument, $oMeta, $oDocument, $oNode, $pkgNode, $row, $col, $packageCurrent) = Devel::PerlySense::Util::aNamedArg(["rhDataDocument", "oMeta", "oDocument", "oNode", "pkgNode", "row", "col", "packageCurrent"], @_);

    #base class (ISA and Roles)
    
    for my $keyword (qw/ extends with /) {
        # Slightly fragile, especially wrt comments
        if ($pkgNode eq "PPI::Statement") {
            if ($oNode =~ /^ $keyword \s+ (?:qw)? \s* (.+);$/xs) {
                my $modules = $1;
                for my $module (grep { $_ ne "qw" } $modules =~ /([\w:]+)/gs) {
                    $rhDataDocument->{rhNameModuleBase}->{$module}++;
                }
            }
        }
    }

    return(1);
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
