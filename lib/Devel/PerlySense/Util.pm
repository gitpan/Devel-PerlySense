=head1 NAME

Devel::PerlySense::Util - Utility routines

=cut



package Devel::PerlySense::Util;
use base "Exporter";

our @EXPORT = (
    qw/
       debug
       slurp
       /);

our $VERSION = '0.01';





use strict;
use warnings;
use Carp;
use Data::Dumper;
use File::Basename;
use Path::Class;





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





=head2 debug($message)

Log debug $message.

Return 1.

=cut
my $fileDebug = "./debug.log";  ###TODO: Change to be bound to project root
sub debug {
	my ($message) = @_;
    return 1;  ##Temporarily disabled until the logs aren't spread out
               ##all over the file system
    open(my $fh, ">>", $fileDebug) or return warn("Could not open ($fileDebug) for append\n");
    $fh->print(localtime() . ": $message\n");
    
    return(1);
}





=head2 slurp($file)

Read the contents of $file and return it, or undef if the file
couldn't be opened.

=cut
sub slurp {
	my ($file) = @_;
    open(my $fh, "<", $file) or return undef;
    local $/;
    return <$fh>;
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
