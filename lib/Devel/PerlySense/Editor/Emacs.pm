=head1 NAME

Devel::PerlySense::Editor::Emacs - Integration with Emacs

=head1 DESCRIPTION


=cut





package Devel::PerlySense::Editor::Emacs;

our $VERSION = '0.01';





use strict;
use warnings;
use Spiffy -Base;
use Data::Dumper;
use File::Basename;
use Graph::Easy;
use Text::Table;
use List::Util qw/ max /;
use POSIX qw/ ceil /;

use Devel::PerlySense;
use Devel::PerlySense::Class;
use Devel::PerlySense::Util;





=head1 PROPERTIES

=head2 oPerlySense

Devel::PerlySense object.

Default: set during new()

=cut
field "oPerlySense" => undef;





=head2 widthDisplay

The width of the display in columns, or undef if N/A.

Default: undef

=cut
field "widthDisplay" => undef;





=head1 API METHODS

=head2 new(oPerlySense, widthDisplay = undef)

Create new Emcacs object.

=cut
sub new {
    my ($oPerlySense, $widthDisplay) = Devel::PerlySense::Util::aNamedArg(["oPerlySense", "widthDisplay"], @_);

    $self = bless {}, $self;    #Create the object. It looks weird because of Spiffy
    $self->oPerlySense($oPerlySense);
    $self->widthDisplay($widthDisplay);

    return($self);
}





=head2 classOverview(oClass)

Return string representing the Class Overview of $oClass.

=cut
sub classOverview {
    my ($oClass) = Devel::PerlySense::Util::aNamedArg(["oClass"], @_);

    my $textInheritance =
            "* Inheritance *\n" .
            $self->textClassInheritance(oClass => $oClass);

    my $textUses =
            "* Uses *\n" .
            $self->textClassUses(oClass => $oClass);

    my $textNeighbourhood =
            "* NeighbourHood *\n" .
            $self->textClassNeighbourhood(oClass => $oClass);

    my $textStructure =
            "* Structure *\n" .
            $self->textClassStructure(oClass => $oClass);


    my $textOverview = join(
        "\n",
        $textInheritance,
        $textUses,
        $textNeighbourhood,
        $textStructure,
    );

    #Highlight the current class
    my $leftBracket = "[[]";
    my $space = "[ ]";
    my $name = $oClass->name;
    $textOverview =~ s| $leftBracket \s+ ( $name \s*? ) $space ] |[<$1>]|xg;

    return $textOverview;
}



=head2 textClassInheritance(oClass)

Return string representing the class hierarchy of $oClass.

=cut
sub textClassInheritance {
    my ($oClass) = Devel::PerlySense::Util::aNamedArg(["oClass"], @_);

    my $oGraph = Graph::Easy->new();
    $oGraph->set_attribute('graph', flow => "up");
    $oGraph->set_attribute('node', border => "dotted");

    $oGraph->add_node($oClass->name);
    my $rhSeenEdge = { };
    $self->addBaseClassNameToGraph(
        oGraph => $oGraph,
        oClass => $oClass,
        rhSeenEdge => $rhSeenEdge,
    );


    # Disable the subclass view until it either becomes faster and/or
    # is better rendered. The Neighbourhood view may be quite enough.

#     $self->addSubClassNameToGraph(
#         oGraph => $oGraph,
#         oClass => $oClass,
#         rhSeenEdge => $rhSeenEdge,
#     );

    my $textInheritance = $self->textCompactGraph(text => $oGraph->as_ascii()) . "\n";

    return $textInheritance;
}



sub addBaseClassNameToGraph {
    my ($oClass, $oGraph, $rhSeenEdge) = Devel::PerlySense::Util::aNamedArg(["oClass", "oGraph", "rhSeenEdge"], @_);

    for my $oClassBase (values %{$oClass->rhClassBase}) {
        $rhSeenEdge->{$oClass->name . "->" .$oClassBase->name}++ and next;
        $oGraph->add_edge($oClass->name, $oClassBase->name);

        $self->addBaseClassNameToGraph(
            oGraph => $oGraph,
            oClass => $oClassBase,
            rhSeenEdge => $rhSeenEdge,
        );
    }

    return 1;
}





sub addSubClassNameToGraph {
    my ($oClass, $oGraph, $rhSeenEdge) = Devel::PerlySense::Util::aNamedArg(["oClass", "oGraph", "rhSeenEdge"], @_);

    for my $oClassSub (values %{$oClass->rhClassSub}) {
        $rhSeenEdge->{$oClassSub->name . "->" .$oClass->name}++ and next;
        $oGraph->add_edge($oClassSub->name, $oClass->name);

        $self->addSubClassNameToGraph(
            oGraph => $oGraph,
            oClass => $oClassSub,
            rhSeenEdge => $rhSeenEdge,
        );
    }

    return 1;
}





=head2 textClassNeighbourhood(oClass)

Return string representing the neighbourhood of $oClass.

=cut
sub textClassNeighbourhood {
    my ($oClass) = Devel::PerlySense::Util::aNamedArg(["oClass"], @_);

    my $rhDirClass = $oClass->rhDirNameClassInNeighbourhood();

    my @aColText;
    for my $raNameClass (map { $rhDirClass->{$_} } qw/ up current down /) {
        my $lenMax = max( map { length } @$raNameClass );

        my $text = join(
            "\n",
            map { sprintf("[ %-*s ]", $lenMax, $_) } @$raNameClass,
        ) || "-none-";

        push(@aColText, $text);
    }

    my $oTable = Text::Table->new();
    $oTable->load([ @aColText ]);

    return "$oTable";
}





=head2 textClassUses(oClass)

Return string representing the modules used by $oClass. Use the least
number of columns to display this.

=cut
sub textClassUses {
    my ($oClass) = Devel::PerlySense::Util::aNamedArg(["oClass"], @_);

    my $columnsToFitWithin = $self->widthDisplay || 90;  ###TODO: Move to config

    my @aNameModule = $oClass->aNameModuleUse();


    my $text = "";
    for my $columns (reverse 1 .. @aNameModule) {
        my @aColText;

        for my $raItem ( @{$self->raItemInNGroups(\@aNameModule, $columns)} ) {
            my $lenMax = max( map { length } @$raItem );

            my $text = join(
                "\n",
                map { sprintf("[ %-*s ]", $lenMax, $_) } @$raItem,
            );

            push(@aColText, $text);
        }

        my $oTable = Text::Table->new();
        $oTable->load([ @aColText ]);
        $text = "$oTable";

        length( (split(/\n/, $text))[0] ) <= $columnsToFitWithin and last;
    }


    return $text;
}





=head2 textClassStructure(oClass)

Return string representing the structure of $oClass.

This includes a Signature Survey string.

=cut
sub textClassStructure {
    my ($oClass) = Devel::PerlySense::Util::aNamedArg(["oClass"], @_);
    
    my $textSignature = $self->textLineWrapped(
        join(
            "",
            map { $_->stringSignatureSurveyFromFile } @{$oClass->raDocument},
        ),
    );

    return "$textSignature\n";
}





=head2 textLineWrapped($text)

Return $text wrapped hard at the available number of columns.

=cut
sub textLineWrapped {
    my ($text) = @_;
    
    my $columnsToFitWithin = $self->widthDisplay || 90;  ###TODO: Move to config
    
    my @aLine;
    while (length($text)) {
        push(@aLine, substr($text, 0, $columnsToFitWithin, ""));
    }

    my $textWrapped = join("\n", @aLine);

    return $textWrapped;
}





=head2 raItemInNGroups($raItem, $countGroup)

Split up the items in $raItem so that they form at most $countGroup
array refs.

The items are evenly distributed between the group with the same numer
of items in each, except for the last one which may contain fewer
items.

Return array ref with $countGroup items, each of which is an array ref
with the elements in $raItem.

=cut
sub raItemInNGroups {
    my ($raItem, $countGroup) = @_;

    my @aItem = @$raItem;
    my $countItemPerGroup = ceil(@aItem / $countGroup) or return( [ ] );

    my @aGroupItem;
    while(scalar @aItem) {
        push(@aGroupItem, [ splice(@aItem, 0, $countItemPerGroup) ]);
    }
    @aItem and push(@aGroupItem, [ @aItem ]);

    return [ @aGroupItem ];
}





=head2 textCompactGraph(text)

Return compact version of $text.

=cut
sub textCompactGraph {
    my ($text) = Devel::PerlySense::Util::aNamedArg(["text"], @_);

    debug($text);
#    warn($text);
    my @aLine = split(/\n/, $text);

    #Remove blank lines
    @aLine = grep { $_ } @aLine;

    #Put [ Class::Name ] around module names
    s{ : ( \s+ [\w:]+ \s+ ) : }{[$1]}xg for (@aLine);

    #Make [ Class::Name ] left-aligned in the box
    my $leftBracket = "[[]";
    my $space = "[ ]";
    s{ $leftBracket $space (\s+) ([\w:]+) }{[ $2$1}xg for (@aLine);

    #Remove border-only lines
    @aLine = grep { ! /[.]/ } @aLine;

    #Remove vertical-lines-only lines
    @aLine = grep { /[^ |^]/ } @aLine;

    $text = join("\n", @aLine);

    return $text;
}





=head2 formatOutputDataStructure(rhData)

Return stringification of $rhData suited for the Editor.

=cut
sub formatOutputDataStructure {
    my ($rhData) = Devel::PerlySense::Util::aNamedArg(["rhData"], @_);

#    return q|'(("class-overview" . "Hej baberiba\n [ Class::Accessor ]") ("class-name" . "Class::Accessor") ("message" . "Whatever2"))|;


    my $keysValues = join(
        " ",
        map {
            my $key = $_;
            my $value = $rhData->{$_};

            $key = $self->renameIdentifier($key);
            $key = $self->escapeValue($key);

            $value = $self->escapeValue($value);

            qq|("$key"| . " . " . qq|"$value")|;
        }
        sort keys %$rhData
    );

    return qq|'($keysValues)|;
}





=head2 renameIdentifier($identifier)

Return $identifier with _ replaced with - to make them more Lispish.

=cut
sub renameIdentifier {
    my ($identifier) = (@_);

    $identifier =~ s/_/-/g;

    return $identifier;
}





###TODO: escape " and \ and fix newlines
sub escapeValue {
    my ($value) = (@_);

    $value =~ s| ([\\"]) |\\$1|gsmx;

    return $value;
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
