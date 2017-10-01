#!/usr/bin/perl

# This script parses TeX files to enumerate references to results

use strict;

use Data::Dumper;

sub readfile {
  my $filename = shift;
  local $/ = undef;
  open my $f, $filename or die "Cannot open $filename: $1";
  my $contents = <$f>;
  close($f);
  return $contents;
}

my %References; # 'From' => ['To', ... ]

sub process_file {
  my $filename = shift;
  
  print "Processing $filename\n";
  
  my $contents = readfile($filename);

  my $LastLabel = "___";
  my $NonCorLabel = "___";
  my $process_block = sub {
    my $type1 = shift;
    my $contents = shift;
    my $type2 = shift;
    
    my $contents2 = substr($contents, 0, 40); # excerpt for logging
    $contents2 =~ s/\s+/ /g;
    
    die "type mismatch '$type1' '$type2' '$contents'" unless $type1 eq $type2;

    my $label;
    unless($type1 eq 'proof') {
      if($contents =~ /\\label\{([^\}]+)\}/){ $label = $1; }
    }
    
    print "Found '$type1' '$label' '$contents2'\n";
    
    die "without label" if $type1 =~ /^my(cor|lem|th|ax)$/ && ! $label;
    
    $LastLabel = $label if $label;
    $NonCorLabel = $label if $label && $type1 ne 'mycor';
    
    if($label && $References{$LastLabel}){
      die "duplicate label";
    }
    
    $References{$LastLabel} //= [];
    
    $contents =~ s/\\cref\{([^\}]+)\}/do{
      my $refs = $1;
      my @refs = split(m!,!, $refs);
      push @{$References{$LastLabel}}, @refs;
    }/ge;
    
    # A corollary depends on the previous result
    if($type1 eq 'mycor'){
      die "corollary without previous result" unless $NonCorLabel;
      push @{$References{$LastLabel}}, $NonCorLabel;
    }
    
  };

  $contents =~ s/\\begin\{(proof|my[a-z]+)\}(.*?)\\end\{(proof|my[a-z]+)\}/do{
    $process_block->($1, $2, $3);
    '';
  }/ges; 
}

sub process_index {
  my $filename = shift;
  
  my $contents = readfile($filename);
  $contents =~ s/\\input\{([^\}]+)\}/do{
    process_file($1.'.tex');
    '';
  }/ge;
}

process_index('paper.tex');
# print Dumper(\%References);

my %Backreferences; # To => From
keys %References;
while(my($from, $tos) = each %References) {
  $Backreferences{$from} //= [];
  foreach my $to (@$tos) {
    push @{$Backreferences{$to} //= []}, $from;
  }
}

print Dumper(\%Backreferences);
