=head1 NAME

Devel::TimeThis - Time the duration of a variable until it goes out of
scope


=head1 DESCRIPTION



=head1 SYNOPSIS



=cut





package Devel::TimeThis;

our $VERSION = '0.01';





use strict;
use warnings;
use Carp;
use Data::Dumper;
use Time::HiRes qw/time/;




=head1 PROPERTIES

=head2 timeStart


=cut




my $rhNameInfo = {};
        



=head1 API METHODS

=head2 new($name)

Create new TimeThis object.

=cut
sub new() {
    my $self = bless {}, shift;
    my ($name) = @_;

    $self->{timeStart} = time();
    $self->{name} = $name;

    return($self);
}





=head2 DESTROY

Collect the timing data

=cut
sub DESTROY {
	my ($self) = @_;

    my $timeDuration = time() - $self->{timeStart};

    $rhNameInfo->{$self->{name}}->{timeDurationAcc} += $timeDuration;
    $rhNameInfo->{$self->{name}}->{count}++;
    $rhNameInfo->{$self->{name}}->{name} = $self->{name};
}




sub END {
    keys %$rhNameInfo and print qq{

* Timing info *

};
    for my $rhInfo (
        sort { $b->{timeDurationAcc} <=> $a->{timeDurationAcc} }
                values %$rhNameInfo)
            {
        printf("% 40s: % 4d : %3.5f\n", $rhInfo->{name}, $rhInfo->{count}, $rhInfo->{timeDurationAcc});       
    }
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


=head2 KNOWN BUGS

PPI is kinda slow for large documents. Lots of objects being created etc.

There are certainly edge cases. Bug reports with failing tests
appreciated :)


=head1 ACKNOWLEDGEMENTS

Peter Liljenberg for his elisp fu.


=head1 COPYRIGHT & LICENSE

Copyright 2005 Johan Lindström, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
