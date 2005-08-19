=head1 NAME

Devel::PerlySense::Util - Utility routines

=cut



package Devel::PerlySense::Util;

our $VERSION = '0.01';





use strict;
use warnings;
use Carp;
use Data::Dumper;





=head1 ROUTINES

=head2 aNamedArg($raParam, @aArg)

Return list of argument valies in $rhArg for the param names in
$raParam.

Die on missing arguments.

=cut
sub aNamedArg {
	my ($raParam, @aArg) = @_;
    my %hArg = @aArg;

    my @aResult;
    for my $param (@$raParam) {
        exists $hArg{$param} or do { local $Carp::CarpLevel = 1; croak("Missing argument ($param). Arguments: " . Dumper(\@_)); };
        push(@aResult, $hArg{$param});
    }

    return(@aResult);
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
