=head1 NAME

Devel::PerlySense - IntelliSense for Perl


=head1 DESCRIPTION

PerlySense is an IntelliSense style utility for editors.

Conveniently navigate and browse the code and documentation of your
project and Perl installation.



=head1 SYNOPSIS


=head2 From Emacs

C-p C-c -- Class Overview -- Show information about the Class at point
or the current Class.

C-p C-d -- Smart docs -- Show docs (POD/signature/etc) for the symbol
(module/method/sub) at point. A doc hint is displayed in the message
area (for methods and subs), or a new POD buffer is created (for
modules).

C-p C-g -- Smart go to -- Open file at proper location for module,
method/sub declaration for the symbol (module/method/sub) at point. If
no sub declaration is available (like for generated getters/setters),
any appropriate POD is used instead.

C-p m f -- Perl Module open File -- Open the source file of the module
at point.

As you can see, PerlySense duplicates some of the functionality in
cperl-mode, and does more in certain areas.



=head2 From Vim

There is no integraton with Vim available. Well, yet.

Someone may want to write it.



=head2 From other editors

Any editor that is programmable and that can call a shell script could
take advantage of PerlySense to implement something similar to the
Emacs functionality. And most editors are programmable by the authors,
if not by the users.



=head2 From the command line

This is not very convenient unless you let your editor do it. See the
L<bin/perly_sense> script on how to do this.

There is one useful command though; cache all the modules in @INC:

    perly_sense process_inc



=head2 From Perl

See the source of the L<bin/perly_sense> script, or the t directory.



=head1 INSTALLATION

Install required modules from CPAN.

=head2 Emacs installation

Copy the file editors/emacs/perly-sense.el to your Emacs script dir
(which should be in the load-path), and add this to the end of your .emacs config file:

  ; PerlySense
  (load "perly-sense")
  (global-unset-key "\C-p")
  (global-set-key (kbd "\C-p \C-d") 'perly-sense-smart-docs-at-point)
  (global-set-key (kbd "\C-p \C-g") 'perly-sense-smart-go-to-at-point)

Adjust according to preference: the key mappings will replace the C-p
command which obviously be fantastically annoying for you if you don't
use the arrow keys to move around. This is just how I use Emacs, and I
expect this will make you go *&&*%!

Calm down. The default key bindings will be changed before the first
real release. Suggestions welcome.



=head1 GETTING STARTED WITH EMACS

Open one of your Perl source files.


=head2 Class Overview

Pressing C-p C-c will bring up the Class Overview of the Class name at
point, or otherwise the current Class (the active Package).

When in the Class Overview buffer:

g -- Go to the thing at point. RET does the same.

d -- Documentation for the thing at point.

c -- Class Overview for the thing at point.

I -- Move point to the Inheritance heading in the buffer.

M -- Move point to the Methods heading in the buffer.

N -- Move point to the 'new' method in the buffer (if any).

q -- Quit the Class Overview buffer.



=head2 Smart docs

C-p C-d is the "Smart docs" command. It brings up documentation for
what's at point.

Put the cursor on the "method" word of a $self->method call and press
C-p C-d and wait until a documentation hint for the method call is
displayed briefly in the message buffer. PerlySense will look in base
classes if the method can't be found in the current class.

Put the cursor on the "method" word of an $object->method call and
press C-p C-d to see the docs hint. PerlySense will look through all
your "use" modules (and their base classes) for the method call and
try to identify the best match.

Note! The first time each module is parsed this will take a second or
two, and the very first time you run the command with lots of "use"
modules it's bound to take longer than that.

Put the cursor on a module name and press C-p C-d to bring up a new
buffer with the POD for that module (this is similar to the cperl-mode
feature, only a) not as good, but b) it works on Windows).

Press C-p C-d with nothing under the cursor brings up a POD buffer for
the current file.


=head2 Smart go to

C-p C-g is the "Smart go to" command. It's similar to Smart Docs, but
instead of bringing the docs to you, it brings you to the definition
of what's at point.

The definition can be either the sub declaration, or if the
declaration can't be found (like for auto-generated getters/setters,
autoloaded subs etc), the POD documentation for the sub.

Before you go anywhere the mark is set. Go back to earlier marks
globally with C-x C-SPC, or locally with C-u C-SPC.




=head1 ON PARSING PERL

Oh, that old topic again...

Well, since Perl is so dynamic, a perfect static analysis of the
source is impossible. But not unusably so. Well, hopefully.

Because of this PerlySense is not about exact rules, but about
heuristics and a 90% solution that isn't perfect, but
good-enough. Sometimes when PerlySense can't make a decision, you're
expected to chip in and tell it what you meant.

If it works for you, brilliant, use it to be more productive.

PerlySense tries to take advantage of the fact that Perl code is more
than the plain source. The source lives in a context of POD and a
directory structure and... well, oher source code.



=head1 SEE ALSO

L<sepia> - similar effort

L<PPI> - excellent for parsing Perl

L<CPANXR> - also uses PPI for cross referencing the CPAN

L<http://www.DarSerMan.com/Perl/Oasis/> - Win32 class
browser/IDE. Earlier (a lot) work by me.



=head1 AUTHOR

Johan Lindström, C<< <johanl[ÄT]DarSerMan.com> >>

=head1 BUGS AND CAVEATS

=head2 BUG REPORTS

Please report any bugs or feature requests to
C<bug-devel-perlysense@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Devel-PerlySense>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.


=head2 CAVEATS

Tab/space isn't supported by PPI yet, but it's supposed to be. So
using Tab instead of spaces won't work properly.



=head2 KNOWN BUGS

PPI is kinda slow for large documents. Lots of objects being created etc.

There are certainly edge cases. Bug reports with failing tests
appreciated :)


=head1 ACKNOWLEDGEMENTS

Peter Liljenberg for his elisp fu.


=head1 COPYRIGHT & LICENSE

Copyright 2007 Johan Lindström, All Rights Reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut





package Devel::PerlySense;

our $VERSION = '0.01_10';





use strict;
use warnings;
use Spiffy -Base;
use Carp;
use Data::Dumper;
use File::Basename;
use File::Path;
use File::Find::Rule;
use Path::Class qw/dir file/;
use Pod::Text;
use IO::String;
use Cache::Cache;
use Storable qw/freeze thaw/;

use Devel::PerlySense::Util;
use Devel::PerlySense::Class;
use Devel::PerlySense::Document;
use Devel::PerlySense::Document::Location;





=head1 *** THE FOLLOWING IS DEVELOPER DOCUMENTATION ***

=head1 PROPERTIES

=head2 oCache

Cache::Cache object, or undef if no cache is active.

Default: undef

=cut
field "oCache" => undef;





=head1 API METHODS

=head2 new()

Create new PerlySense object.

=cut
sub new() {
    my $self = bless {}, shift;

    return($self);
}





=head2 oDocumentParseFile($file)

Parse $file into a new PerlySense::Document object.

Return the new object.

Die on errors (like if the file wasn't found).

=cut
sub oDocumentParseFile {
	my ($file) = @_;

    my $oDocument = Devel::PerlySense::Document->new(oPerlySense => $self);
    $oDocument->parse(file => $file);

    return($oDocument);
}





=head2 podFromFile(file => $file)

Return the pod in $file as text, or die on errors.

Die if $file doesn't exist.

=cut
sub podFromFile {
    my ($file) = Devel::PerlySense::Util::aNamedArg(["file"], @_);

    open(my $fhIn, $file) or die("Could not open file ($file): $!\n");

    my $textPod = "";
    my $fhOut = IO::String->new($textPod);
    Pod::Text->new()->parse_from_filehandle($fhIn, $fhOut);

    return($textPod);
}





=head2 oLocationSmartGoTo(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what is
there. Depending on what's there, find the source
declaration/whatever, find it and return an
Devel::PerlySense::Document::Location object.

Currently supported:

  $self->method, look in current file and base classes. If no sub can
  be found, look for POD.

  $object->method, look in current file and used modules. If no sub
  can be found, look for POD.

  Module::Name (bareword)

  Module::Name (as the only contents of a string literal)

If there's nothing at $row/col, or if the source can't be found,
return undef.

Die if $file doesn't exist, or on other errors.

=cut
sub oLocationSmartGoTo {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);
    debug("oLocationSmartGoTo file($file) row($row) col($col)");

    my $oDocument = $self->oDocumentParseFile($file);

    {
        if(my $method = $oDocument->selfMethodCallAt(row => $row, col => $col)) {
            my $oLocation = $oDocument->oLocationSubDefinition(row => $row, name => $method);
            $oLocation and return($oLocation);
        }
    }

    my ($module, $method) = $oDocument->moduleMethodCallAt(row => $row, col => $col);
    if($module && $method) {
        if(my $oDocumentDest = $self->oDocumentFindModule(nameModule => $module, dirOrigin => dirname($file))) {
            my $oLocation = $oDocumentDest->oLocationSubDefinition(row => $row, name => $method);
            $oLocation and return($oLocation);
        }
    }


    my ($oObject, $oMethod, $oLocationSub) = $oDocument->aObjectMethodCallAt(row => $row, col => $col);
    if($oObject && $oMethod && $oLocationSub) {
        debug("Looking for $oObject->$oMethod");
        my @aMethodCall = $oDocument->aMethodCallOf(
            nameObject => "$oObject",
            oLocationWithin => $oLocationSub,
        );
        my @aNameModuleUse = $oDocument->aNameModuleUse();  #Add all known modules, not just the ones explicitly stated
        my @aDocumentDest = $self->aDocumentFindModuleWithInterface(
            raNameModule => \@aNameModuleUse,
            raMethodRequired => [ "$oMethod" ] ,
            raMethodNice => \@aMethodCall,
            dirOrigin => dirname($file),
        );
        if(@aDocumentDest) {
            debug("Possible matching modules:\n" . join("\n", map { "  * $_" } map { @{$_->oMeta->raPackage} } @aDocumentDest));
            my $oLocation = $aDocumentDest[0]->oLocationSubDefinition(
                row => $row,
                name => "$oMethod",
            );
            $oLocation and return($oLocation);
        }
    }


    if(my $module = $oDocument->moduleAt(row => $row, col => $col)) {
        my $file = $self->fileFindModule(nameModule => $module, dirOrigin => dirname($file))
                or return(undef);

        my $oLocation = Devel::PerlySense::Document::Location->new(file => $file, row => 1, col => 1);
        return($oLocation);
    }

    return(undef);
}





=head2 oLocationSmartDoc(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what is
there. Depending on what's there, find the documentation for it and
return a Document::Location object with the following rhProperty keys set:

  text - the docs text
  found - "method" | "module"
  docType - "hint" | "document"
  name - the name of the thing found


Currently supported:

  Same as for oLocationSmartGoTo

If there's nothing at $row/col, use the current document.

Die if $file doesn't exist, or on other errors.

=cut
#Rework this so it can deal with HTML output as well
sub oLocationSmartDoc {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    my $oDocument = $self->oDocumentParseFile($file);

    my $oLocation = undef;
    if(my $method = $oDocument->selfMethodCallAt(row => $row, col => $col)) {
        $oLocation = $oDocument->oLocationPod(name => $method, lookFor => "method");
        return( $self->oLocationRenderPodToText($oLocation) );
    }

    my ($module, $method) = $oDocument->moduleMethodCallAt(row => $row, col => $col);
    if($module && $method) {
        if(my $oDocumentDest = $self->oDocumentFindModule(nameModule => $module, dirOrigin => dirname($file))) {
            my $oLocation = $oDocumentDest->oLocationPod(name => $method, lookFor => "method");
            return( $self->oLocationRenderPodToText($oLocation) );
        }
    }


    my ($oObject, $oMethod, $oLocationSub) = $oDocument->aObjectMethodCallAt(row => $row, col => $col);
    if($oObject && $oMethod && $oLocationSub) {
        my @aMethodCall = $oDocument->aMethodCallOf(nameObject => "$oObject", oLocationWithin => $oLocationSub);
        my @aNameModuleUse = $oDocument->aNameModuleUse();
        my @aDocumentDest = $self->aDocumentFindModuleWithInterface(raNameModule => \@aNameModuleUse, raMethodRequired => [ "$oMethod" ] , raMethodNice => \@aMethodCall, dirOrigin => dirname($file));
        if(@aDocumentDest) {
            my $oLocation = $aDocumentDest[0]->oLocationPod(name => "$oMethod", lookFor => "method");
            return( $self->oLocationRenderPodToText($oLocation) );
        }
    }


    if(my $module = $oDocument->moduleAt(row => $row, col => $col)) {
        my $file = $self->fileFindModule(nameModule => $module, dirOrigin => dirname($file))
                or return(undef);

        my $oLocation = Devel::PerlySense::Document::Location->new(file => $file, row => 1, col => 1);
        $oLocation->rhProperty->{found} = "module";
        $oLocation->rhProperty->{docType} = "document";
        $oLocation->rhProperty->{name} = "$module";
        $oLocation->rhProperty->{text} = $self->podFromFile(file => $file) or return(undef);
        return($oLocation);
    }


    #Fail to docs about this current file
    if($oDocument->isEmptyAt(row => $row, col => $col)) {
        $oLocation = Devel::PerlySense::Document::Location->new(file => $file, row => 1, col => 1);
        $oLocation->rhProperty->{found} = "module";
        $oLocation->rhProperty->{docType} = "document";
        $oLocation->rhProperty->{name} = $oDocument->packageAt(row => $row);
        $oLocation->rhProperty->{text} = $self->podFromFile(file => $file) or return(undef);
        return($oLocation);
    }

    return(undef);
}





=head2 classNameAt(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what class name that is. 

Return the class name or "" if it's package main.

Die if $file doesn't exist, or on other errors.

=cut
sub classNameAt {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    my $oDocument = $self->oDocumentParseFile($file);
    
    my $package = $oDocument->packageAt(row => $row);

    $package eq "main" and return "";
    return($package);
}





=head2 classAt(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what
PerlySelse::Class that is.

Return the Class object or undef if it's package main.

Die if $file doesn't exist, or on other errors.

=cut
sub classAt {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    return(Devel::PerlySense::Class->newFromFileAt(
        oPerlySense => $self,
        file => $file,
        row => $row,
        col => $col,
    ));
}





=head2 fileFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin)

Find the file containing the $nameModule given the $dirOrigin.

Return the absolute file name, or undef if none could be found. Die on
errors.

=cut
sub fileFindModule {
    my ($nameModule, $dirOrigin) = Devel::PerlySense::Util::aNamedArg(["nameModule", "dirOrigin"], @_);
#my $tt = Devel::TimeThis->new("fileFindModule");
    my $fileModuleBase = $self->fileFromModule($nameModule);
    $dirOrigin = dir($dirOrigin)->absolute;

    return(
        $self->fileFindLookingAround($fileModuleBase, $dirOrigin) ||
        $self->fileFindLookingInInc($fileModuleBase) ||
        undef
    );
}





=head2 oDocumentFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin)

Find the file containing the $nameModule given the $dirOrigin.

Return a parsed PerlySense::Document, or undef if none could be
found. Die on errors.

=cut
sub oDocumentFindModule {
    my ($nameModule, $dirOrigin) = Devel::PerlySense::Util::aNamedArg(["nameModule", "dirOrigin"], @_);

    my $fileModule = $self->fileFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin) or return(undef);
    my $oDocument = $self->oDocumentParseFile($fileModule) or return(undef);

    return($oDocument);
}





=head1 IMPLEMENTATION METHODS

=head2 fileFindLookingAround($fileModuleBase, $dirOrigin)

Find the file containing the $nameModule given the $dirOrigin.

Return the file name relative to $dirOrigin, or undef if none could be
found. Die on errors.

=cut
sub fileFindLookingAround {
	my ($fileModuleBase, $dirOrigin) = @_;

    my $dir = $dirOrigin;
    while(1) {
        for my $dirCur (map { dir($dir, $_) } qw/. bin lib/) {
            if(my $fileFound = $self->fileFoundInDir($dirCur, $fileModuleBase)) {
                return(file($fileFound)->absolute . "");
            }
        }

        $dir = $dir->parent;
        $dir =~ m{^( / | \\ | \w: \\ )$}x and last;  #At the root? Unix/Win32. What filesystems are missing?
    }

    return(undef);
}





=head2 fileFindLookingInInc($fileModuleBase)

Find the file containing the $nameModule in @INC.

Return the absolute file name, or undef if none could be found. Die on
errors.

=cut
sub fileFindLookingInInc {
	my ($fileModuleBase) = @_;

    for my $dirCur (@INC) {
        if(my $fileFound = $self->fileFoundInDir($dirCur, $fileModuleBase)) {
            return($fileFound);
        }
    }

    return(undef);
}





=head2 fileFromModule($nameModule)

Return the $nameModule converted to a file name (i.e. with dirs and
.pm extension).

=cut
sub fileFromModule {
	my ($nameModule) = @_;
    return( file( split(/::/, $nameModule) ) . ".pm" );
}





=head2 fileFoundInDir($dir, $fileModuleBase)

Look if $fileModuleBase is located in $dir. If it is, return the file
name relative to $dirOrigin.

Return the absolute file name, or "" if not found at $dir.

=cut
sub fileFoundInDir {
	my ($dir, $fileModuleBase) = @_;

    my $file = file($dir, $fileModuleBase);
    -e $file and return( $file->absolute . "" );

    return("");
}





=head2 textFromPod($pod)

Return $pod rendered as text, or die on errors.

=cut
sub textFromPod {
	my ($pod) = @_;

    my $text = "";
    my $fhIn = IO::String->new($pod);
    my $fhOut = IO::String->new($text);
    Pod::Text->new()->parse_from_filehandle($fhIn, $fhOut);

    $text =~ s/\s+$//s;

    return($text);
}





=head2 oLocationRenderPodToText($oLocation)

Render the $oLocation->rhProperty->{pod} and put it in
rhProperty->{text}.

Return the same (modified) $oLocation object, or undef if no
rhProperty->{pod} property ended up as text (after this operation,
there is content in rhProperty->{text}).

Return undef if $oLocation is undef.

Die on errors.

=cut
sub oLocationRenderPodToText {
	my ($oLocation) = @_;
    $oLocation or return(undef);

    my $pod = $oLocation->rhProperty->{pod} or return(undef);
    $oLocation->rhProperty->{text} = $self->textFromPod($pod) or return(undef);

    return($oLocation);
}





=head2 aDocumentFindModuleWithInterface(raNameModule => $raNameModule, raMethodRequired => $raMethodRequired, raMethodNice => $raMethodNice, dirOrigin => $dirOrigin)

Return a list with Devel::PerlySense::Document objects that support
all of the methods in $raMethodRequired and possibly the methods in
$raMethodNice. Look in modules in $raNameModule.

The list is sorted with the best match first.

If the document APIs have one or more base classes, look in the @ISA
(depth-first, just like Perl (see perldoc perltoot)).

Warn on some failures to find the location. Die on errors.

=cut
sub aDocumentFindModuleWithInterface {
    my ($raNameModule, $raMethodRequired, $raMethodNice, $dirOrigin) = Devel::PerlySense::Util::aNamedArg(["raNameModule", "raMethodRequired", "raMethodNice", "dirOrigin"], @_);
#my $tt = Devel::TimeThis->new("aDocumentFindModuleWithInterface");

    my @aDocument;
    for my $nameModule (@$raNameModule) {
#print "module: $nameModule\n";
        my $oDocument = $self->oDocumentFindModule(nameModule => $nameModule, dirOrigin => $dirOrigin) or next;
        $oDocument->determineLikelyApi(nameModule => $nameModule) or next;
        my $score = $oDocument->scoreInterfaceMatch(nameModule => $nameModule, raMethodRequired => $raMethodRequired, raMethodNice => $raMethodNice) or next;

        push(@aDocument, { oDocument => $oDocument, score => $score });
    }

    my @aDocumentWithInterface =
            map { $_->{oDocument} }
            sort { $a->{score} <=> $b->{score} }
            @aDocument;

    return(@aDocumentWithInterface);
}





=head2 aApiOfClass(file => $fileOrigin, row => $row, col => $row)

Look in $file at location $row/$col and determine what package is
there.

Return a two item array with (Package name,
Devel::PerlySense::Document::Api object with the likely API of that
class), or () if none was found.

Die if $file doesn't exist, or on other errors.

=cut
sub aApiOfClass {
    my ($file, $row, $col) = Devel::PerlySense::Util::aNamedArg(["file", "row", "col"], @_);

    my $oDocument = $self->oDocumentParseFile($file);
    my $packageName = $oDocument->packageAt(row => $row) or return(undef);

    $oDocument->determineLikelyApi(nameModule => $packageName) or return(undef);

    return($packageName, $oDocument->rhPackageApiLikely->{$packageName});
}





=head2 aDocumentGrepInDir(dir => $dir, rsGrepFile => $rsGrepFile, rsGrepDocument => $rsGrepDocument)

Return a list with Devel::PerlySense::Document objects found under the
$dir, and that return true for the grep sub $rsGrepFile and $rsGrepDocument.

If any found file couldn't be parsed, skip it silently from the list.

=cut
sub aDocumentGrepInDir {
    my ($dir, $rsGrepFile, $rsGrepDocument) = Devel::PerlySense::Util::aNamedArg(["dir", "rsGrepFile", "rsGrepDocument"], @_);

    my @aDocument =
            map {
                my $oDocument = Devel::PerlySense::Document->new(oPerlySense => $self);
                eval { $oDocument->parse(file => $_) };
                $@ ?
                    () :
                    $rsGrepDocument->($oDocument) ?
                        $oDocument :
                        ();
            }
            grep { $rsGrepFile->($_) }
            File::Find::Rule->file->name("*.pm")->in($dir);

    return(@aDocument);
}





=head1 CACHE METHODS


=head2 cacheSet(file => $file, key => $key, value => $valuex)

If the oCache isn't undef, store the $value in the cache under the
total key of ($file, $file's timestamp, $key, and the PerlySense
VERSION).

$value should be a scalar or reference which can be freezed.

$file must be an existing file.

Return 1 if the $value was stored, else 0. Die on errors.

=cut
#Move these to Devel::PerlySense::Util::Cache ?
sub cacheSet {
    my ($file, $key, $value) = Devel::PerlySense::Util::aNamedArg(["file", "key", "value"], @_);

    my $keyTotal = $self->cacheKeyTotal($file, $key) or return(0);

    my $data = freeze($value) or return(0);
    $self->oCache->set($keyTotal, $data);

    return(1);
}





=head2 cacheGet(file => $file, key => $key)

If the oCache isn't undef, get the value in the cache under the total
key of ($file, $file's timestamp, $key) and return it.

$file must be an existing file.

Return the value, or undef if the value could not be fetched. Die on errors.

=cut
sub cacheGet {
    my ($file, $key) = Devel::PerlySense::Util::aNamedArg(["file", "key"], @_);

    my $keyTotal = $self->cacheKeyTotal($file, $key) or
#            warn("Could not get key for ($file) ($key)\n"),
                    return(undef);

    my $data = $self->oCache->get($keyTotal) or
#            warn("?\n"),
                    return(undef);
#warn("!\n");

    my $rValue = thaw($data) or warn("Could not thaw\n"), return(undef);
    return( $rValue );
}





=head2 cacheKeyTotal($file, $key)

If oCache is undef, return undef.

Otherwise, return the total key of ($file, $file's timestamp, $key,
and the PerlySense VERSION).

$file must be an existing file.

Die on errors.

=cut
sub cacheKeyTotal {
    my ($file, $key) = @_;
    $self->oCache or return(undef);

    my $timestamp = (stat($file))[9] or die("Could not read timestamp for file ($file)\n");
    my $keyTotal = join("\t", $file, $timestamp, $key, $self->VERSION);

    return($keyTotal);
}





1;





__END__
