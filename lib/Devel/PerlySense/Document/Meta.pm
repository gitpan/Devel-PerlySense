=head1 NAME

Devel::PerlySense::Document::Meta - information generated during a parse

=cut





package Devel::PerlySense::Document::Meta;

our $VERSION = '0.01';





use strict;
use warnings;
use Spiffy -Base;
use Carp;
use Data::Dumper;
use PPI::Document;





=head1 PROPERTIES

=head2 raPackage

Package declarations.

Array ref with cloned PPI::Statement::Package objects.

Default: []

=cut
field "raPackage" => [];





=head2 raNameModuleUse

Array ref with module names that are "use"d.

Default: []

=cut
field "raNameModuleUse" => [];





=head2 raNameModuleBase

Array ref with module names that are base classes.

Default: []

=cut
field "raNameModuleBase" => [];





=head2 rhRowColModule

Module names.

Hash ref with (keys: row, values:
  hash ref with (keys: col, values:
    cloned PPI::Node objects
  )
)

rhRowColModule->{43}->{2}-> node

Default: {}

=cut
field "rhRowColModule" => {};





=head2 rhRowColMethod

Method calls.

Hash ref with (keys: row, values:
  hash ref with (keys: col, values:
    {
    oNode => cloned PPI::Node object,
    oNodePrev => node to the left of the ->
    }
  )
)

rhRowColModule->{43}->{2}-> node

Default: {}

=cut
field "rhRowColMethod" => {};





=head2 raLocationPod

POD blocks.

Array ref with Location objects, representing each pod chunk that is a
heading/item. They have the following properties:

  podSection
  pod

Default: []

=cut
field "raLocationPod" => [];





=head2 raLocationSub

sub definition.

Array ref with Location objects, representing each sub
declaration. They have the following properties:

  nameSub
  source
  oLocationEnd

Default: []

=cut
field "raLocationSub" => [];





=head1 API METHODS

=head2 new()

Create new empty Meta object

=cut
sub new(@) {
    my $pkg = shift;

    my $self = bless {}, $pkg;

    return($self);
}





=head2 parse($oDocument)

Parse the Devel::PerlySense::Document and extract metadata. Fill
appropriate data structures.

Return 1 or die on errors.

=cut
sub parse {
    my ($oDocument) = @_;

    my @aToken;
    my @aPackage;
    my %hNameModuleUse;
    my %hNameModuleBase;
    my %hRowColModule;
    my %hRowColMethod;
    my @aLocationPod;
    my @aPodHeadingCurrent;
    my $packageCurrent = "main";

    $oDocument->aDocumentFind(
        sub {
            my ($oTop, $oNode) = @_;
            my $oLocation = $oNode->location or return(0);

            eval {

                my ($row, $col) = ($oLocation->[0], $oLocation->[1]);
                my $pkgNode = ref($oNode);    #Optimization: compare against the string instead of doing insanely many ->isa(). This is slightly fragile wrt changes in subclasses in PPI.



                #Collect tokens
                if($pkgNode =~ /^PPI::Token/ && $oNode->location) {
                    if($pkgNode =~ /^PPI::Token::QuoteLike/ || $pkgNode =~ /^PPI::Token::Quote/) {
                        push(@aToken, $oNode);
                    } else {
                        #...we're only interested in nodes which are single words
                        if( $oNode !~ /\s/) {
                            push(@aToken, $oNode);
                        }
                    }
                }




                #package
                if($pkgNode eq "PPI::Statement::Package") {
                    push(@aPackage, $oNode);
                    $packageCurrent = $oNode->namespace;
                }




                #use
                if($pkgNode eq "PPI::Statement::Include") {
                    $hNameModuleUse{$1}++ if($oNode =~ /^ use \s+ ( [A-Z][\w:]* ) /xs);
                }




                #base class

                # use base
                if($pkgNode eq "PPI::Statement::Include") {
                    if($oNode =~ /^ use \s+ base \s+ qw?\s* (.+);$/xs) {
                        my $modules = $1;
                        $hNameModuleBase{$_}++ for($modules =~ /([\w:]+)/gs);
                    }
                }

                # @ISA = ...
                ## fragile: stuff to the right...
                if($pkgNode eq "PPI::Token::Symbol" && $oNode eq '@ISA') {
                    my $oStatement = $oNode->statement;
                    if($oStatement =~ /\@ISA \s* = \s* (.+);$/xs) {
                        my $modules = $1;
                        $hNameModuleBase{$_}++ for($modules =~ /([\w:]+)/gs);
                    }
                }

                #push(@ISA, )
                ## fragile: "push() if(sdfkjs)" doesn't work
                if($pkgNode eq "PPI::Token::Symbol" && $oNode eq '@ISA' && @aToken > 2) {
                    my $prev = -1; #last one is the '@ISA'

                    if($aToken[--$prev] eq "push" || $aToken[--$prev] eq "push") {
                        my $oStatement = $oNode->parent->parent;
                        $oStatement =~ /\@ISA \s* , \s* (.+)/xs or next;
                        my $modules = $1;

                        $hNameModuleBase{$_}++ for($modules =~ /([\w:]+)/gs);
                    }
                }




                #module
                if($pkgNode eq "PPI::Token::Word" &&
                            $oNode =~ /^[A-Z][\w:]*$/ #Word chars and ::, Starts with uppercase, is pragma or number
                        ) {
                    if( ! ($aToken[-2]->isa("PPI::Token::Operator") && $aToken[-2] eq "->") ) {
                        $hRowColModule{$row}->{$col} = {
                            oNode => $oNode,
                        };
                    }
                }




                #method
                if($pkgNode eq "PPI::Token::Word" && @aToken > 2) {
                    my ($oObject, $oOperator) = @aToken[-3, -2];
                    if($oOperator->isa("PPI::Token::Operator") && $oOperator eq "->") {
                        $oObject->isa("PPI::Token::Symbol") || $oObject->isa("PPI::Token::Word") or $oObject = undef;
#print "$row/$col: ($oObject$oOperator$oNode)\n";
                        $hRowColMethod{$row}->{$col} = {
                            oNode => $oNode,
                            oNodeObject => $oObject,
                        };
                    }

                }



                #pod
                if($pkgNode eq "PPI::Token::Pod") {
                    $self->parsePod($oDocument, $oNode, \@aLocationPod, \@aPodHeadingCurrent);
                }



                #sub
                my $nameSub = "";
                $pkgNode eq "PPI::Statement::Sub" && !$oNode->forward and $nameSub = $oNode->name;
                $pkgNode eq "PPI::Statement::Scheduled" and $nameSub = $oNode->type;
                if($nameSub) {
                    my $oLocation = Devel::PerlySense::Document::Location->new(
                        file => $oDocument->file,
                        row => $oNode->location->[0],
                        col => $oNode->location->[1],
                    );
                    $oLocation->rhProperty->{nameSub} = $nameSub;
                    $oLocation->rhProperty->{source} = "$oNode";
                    $oLocation->rhProperty->{namePackage} = $packageCurrent;
                    

                    my $countNewline =()= $oNode =~ /\n/g;
                    my ($rowEnd, $colEnd) = ($oNode->location->[0] + $countNewline, 1);
                    if($countNewline) {
                        $oNode =~ /\n([^\n]+?)\z/ and $colEnd += length($1);
                    } else {
                        $colEnd = length($oNode);
                    }

                    my $oLocationEnd = Devel::PerlySense::Document::Location->new(
                        file => $oDocument->file,
                        row => $rowEnd,
                        col => $colEnd,
                    );

                    $oLocation->rhProperty->{oLocationEnd} = $oLocationEnd;

                    push(@{$self->raLocationSub}, $oLocation);
                }


            };
            $@ and warn($@);

            return(0);
        });

    $self->raPackage(\@aPackage);
    $self->raNameModuleUse([sort keys %hNameModuleUse]);
    $self->raNameModuleBase([sort keys %hNameModuleBase]);
    $self->rhRowColModule(\%hRowColModule);
    $self->rhRowColMethod(\%hRowColMethod);
    $self->raLocationPod(\@aLocationPod);

    return(1);
}





=head2 moduleAt(row => $row, col => $col)

Find the module mentioned on line $row (1..) at $col (1..).

Return string like "My::Module" or "Module", or undef if none was
found.

=cut
sub moduleAt {
    my ($row, $col) = Devel::PerlySense::Util::aNamedArg(["row", "col"], @_);
    my $rhToken = $self->rhTokenOfAt($self->rhRowColModule, $row, $col) or return(undef);
    return("$rhToken->{oNode}");
}





=head2 rhMethodAt(row => $row, col => $col)

Find the module mentioned on line $row (1..) at $col (1..).

Return hash ref with { oNode, oNodeObject } or undef if none was
found.

=cut
sub rhMethodAt {
    my ($row, $col) = Devel::PerlySense::Util::aNamedArg(["row", "col"], @_);
    return($self->rhTokenOfAt($self->rhRowColMethod, $row, $col));
}





=head2 rhTokenOfAt($rhRowCol, $row, $col)

Find the token mentioned in $rhRowCol on line $row (1..) at $col (1..).

Return hash ref with keys oNode and possibly oNodeObject, or undef if
none was found.

=cut
sub rhTokenOfAt {
    my ($rhRowCol, $row, $col) = @_;

    my $rhCol = $rhRowCol->{$row} or return(undef);
    for my $colToken (keys %$rhCol) {
        my $rhToken = $rhCol->{$colToken};
        my $oNode = $rhToken->{oNode};
        my $colTokenEnd = $colToken + length($oNode);
        if($col >= $colToken && $col < $colTokenEnd) {
            return($rhToken);
        }
    }

    return(undef);
}





=head2 parsePod($oDocument, $oNode, $raLocationPod, $raPodHeadingCurrent)

Parse $oNode and add one or more Location objects to $raLocationPod.

Add pod chunks that are =head or =item. Prefix the pod chunks with
their immediate pod heading level.

Return 1 on success, die on errors.

=cut
sub parsePod {
    my ($oDocument, $oNode, $raLocationPod, $raPodHeadingCurrent) = @_;

    my @aLine = split(/\n/, $oNode);
    my $lineCur = -1;
    for my $line (@aLine) {
        $lineCur++;

        if($line =~ /^ (?: =head(\d+)\b ) | (?: =item\b )/x) {
            my $headingLevel = $1 || 0;
            if($headingLevel) {
                @$raPodHeadingCurrent > $headingLevel and splice(@$raPodHeadingCurrent, $headingLevel);  #Remove everything below this heading
                $raPodHeadingCurrent->[$headingLevel - 1] = $line;
            }

            my $podSection = "";
            my $level = 0;
            for my $heading (@$raPodHeadingCurrent) {
                ($level < $headingLevel - 1) || ($headingLevel == 0)  and $podSection .= "$heading\n\n";
                $level++;
            }


            my $pod = "$line\n";
            my $linePod = $lineCur + 1;
            while(defined($aLine[$linePod]) && $aLine[$linePod] !~ /^=/) {
                $pod .= $aLine[$linePod++] . "\n";
            }

            my $oLocation = Devel::PerlySense::Document::Location->new(
                file => $oDocument->file,
                row => $oNode->location->[0] + $lineCur,
                col => 1,
            );
            $oLocation->rhProperty->{pod} = $pod;
            $oLocation->rhProperty->{podSection} = $podSection;

            push(@$raLocationPod, $oLocation);
        }
    }

    return(1);
}





1;





__END__

=head1 AUTHOR

Johan Lindstr�m, C<< <johanl[�T]DarSerMan.com> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-devel-perlysense@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-PerlySense>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2005 Johan Lindstr�m, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
