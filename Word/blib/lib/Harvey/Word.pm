package Word;

use 5.006;
use strict;
use warnings;
use MLDBM qw(DB_File Storable);
use Fcntl;

our $VERSION = '1.01';

my %t_word;		# Tied word hash	1.01
my %word;		# Cached word hash - cached on demand when
			# new word occurs	1.01
my $hash_tied = 0;	# gets set to 1 when first used 1.01

################### Flags for Prepositions ####################################

my $C_COORD 	= 1;		# coordinating conjunctions 1.01
my $C_SUBORD	= 2;		# subordinating conjunctions 1.01

################### Flags for Prepositions ####################################

my $PR_REGULAR 	= 1;		# Standard preposition 1.01
my $PR_ADVERB	= 2;		# Adverbal preposition 1.01 

################### Flags for Adjectives and Adverbs ##########################

my $A_BASE = 1;				# base form 1.01
my $A_COMPARATIVE = 2;		# comparative adjective 1.01
my $A_SUPERLATIVE = 4;		# superlative adjective 1.01

################### Flags for Determiners #####################################

my $D_QUESTION 	= 	1;	# question/subordinate clause determiner 1.01
my $D_STANDALONE = 	2;	# standalone capable determiner 1.01
my $D_PLURAL	=	4;	# plural capable determiner 1.01
my $D_SINGULAR	=	8;	# singular capable determiner 1.01

################### Flags for Pronouns ###################################

my $PN_FIRST		= 1;		# first person 1.01
my $PN_SECOND		= 2;		# second person 1.01
my $PN_THIRD 		= 4;		# third person 1.01
my $PN_SINGULAR 	= 8;		# singular 1.01
my $PN_PLURAL		= 16;		# plural 1.01
my $PN_MASCULINE	= 32;		# masculine 1.01
my $PN_FEMININE		= 64;		# feminine 1.01
my $PN_NEUTER 		= 128;		# neuter 1.01
my $PN_NOMINATIVE	= 1 << 8;	# nominative 1.01
my $PN_ACCUSATIVE	= 2 << 8;	# accusative 1.01
my $PN_GENITIVE		= 4 << 8;	# genitive 1.01
my $PN_PERSON		= 8 << 8;	# person 1.01
my $PN_PLACE		= 16 << 8;	# place 1.01
my $PN_THING		= 32 << 8;	# thing 1.01
my $PN_REFLEXIVE	= 64 << 8;	# reflexive 1.01
my $PN_QUESTION 	= 128 << 8;	# question word 1.01
my $PN_ADJECTIVAL	= 1 << 16;	# acts as an adjective, my, your 1.01
my $PN_STANDALONE	= 2 << 16;	# can be free standing 1.01

################### Flags for the noun ###################################

my $N_NUMBERLESS	= 1;	# Number not an issue or either 1.01
my $N_SINGULAR		= 2;	# 1.01
my $N_PLURAL		= 4;	# 1.01
my $N_PROPER		= 8;	# 1.01
my $N_STARTER		= 16;	# Can start a Subject 1.01
my $N_MASCULINE		= 32;	# Gender most useful in tracking agreement 1.01
my $N_FEMININE		= 64;	# between Pronouns and Nouns 1.01
my $N_NEUTER		= 128;	# 1.01

################### Flags for the verb ###################################

my $V_INFINITIVE	= 1;	# general verb flags 1.01
my $V_PAST		= 2;	# 1.01
my $V_PARTICIPLE	= 4;	# 1.01
my $V_THIRD		= 8;	# third person sing present 1.01
my $V_GERUND		= 16;	# 1.01
my $V_PRESENT		= 32;	# all other present forms 1.01
my $V_STARTER		= 64;	# can start a verb structure 1.01

################### Flags for Tracking Person implied by word ###########

# Used to track agreement between verbs and nouns.  Used with verb_persons()
# and noun_persons().

my $P_1PS	= 1;		# First Person Singular 1.01
my $P_2PS	= 2;		# Second Person Singular 1.01
my $P_3PS	= 4;		# Third Person Singular 1.01
my $P_1PP	= 8;		# First Person Plural 1.01
my $P_2PP	= 16;		# Second Person Plural 1.01
my $P_3PP	= 32;		# Third Person Plural 1.01
my $P_ALL 	= $P_1PS | $P_2PS | $P_3PS | $P_1PP | $P_2PP | $P_3PP; # 1.01  


# Constructor for Word 
sub new {				# 1.01
	my $class = shift;
	my $self = {};
	bless ($self,$class);

	# Establish tied hashes
	&tie_hash() if (!$hash_tied);	

	# load current word info, otherwise
	# can be loaded later with a call to
	# text() method
	$self->text(shift) if @_;
	$self->prioritize();
	return $self;
}

# Get/Set the Text for this word.  This will be the element
# that governs all other reads or writes to this object
# 1.01
sub text {					# 1.01
	my $self = shift;
	$self->{Text} = shift if (@_);
	# Load record from TIE hash
	$self->load_tie();					
	return $self->{Text};
}

# Cache the tie hash into the local hash if the word exists

sub load_tie {					# 1.01
	my $self = shift;
	my $W = $self->{Text};

	if (!exists $word{$W}) {
		# load record into memory hash if not loaded
		if (exists $t_word{$W}) {
			$word{$W} = $t_word{$W};
		}
		else {
			# create record
			$word{$W}->{Text} = $W;
			$t_word{$W} = $word{$W};
		}
	}
}

# Delete the word record for the current object

sub delete_word {
	my $self = shift;
	my $W = $self->{Text};

	delete $word{$W};
	delete $t_word{$W};
}

# Makes an array of prioritized parts of speech for the word
# based on the BNC frequency count

sub prioritize {				# 1.01
	my $self = shift;
	my %H;
	my $K;
	
	$H{Noun} = $self->noun_freq() if $self->noun();
	$H{Verb} = $self->verb_freq() if $self->verb();
	$H{Adjective} = $self->adjective_freq() if $self->adjective();
	$H{Adverb} = $self->adverb_freq() if $self->adverb();
	$H{Modal} = $self->modal_freq() if $self->modal();
	$H{Pronoun} = $self->pronoun_freq() if $self->pronoun();
	$H{Determiner} = $self->determiner_freq() if $self->determiner();
	$H{Preposition} = $self->preposition_freq() if $self->preposition();
	$H{Conjunction} = $self->conjunction_freq() if $self->conjunction();

	$self->{Priority} = [];

	foreach $K (sort { $H{$a} <=> $H{$b} } keys %H) {
		push @{$self->{Priority}},$K; 
	}
	@{$self->{Priority}} = reverse @{$self->{Priority}};

	return $self->{Priority};
}

############ Part of Speech Section ###################################
#
# The following routines can be used as boolean tests for the part of
# speech of a word.  They return the flag set for the word, which will
# be a positive number (flags must be designed so that at least 1 flag
# is always set) if the word is the requested part of speech.  In this
# way an entire flag set can be gotten into an integer if desired.  
# The entire flag set can also be set by sending the appropriate settings
# into the function as the first argument.
#
#######################################################################

# Basic frame for getting the part of speech.  Called by other part of
# speech functions, like noun, verb, etc.  Not intended to be called 
# directly itself.

sub part_of_speech {			# 1.01
	my $F = shift;			# get the name of the flag set
	my $self = shift;		# got passed on from calling method
	my $W = $self->{Text};	# Get the text of the word

	if (@_) {				# Are we setting the flags?
		# Get the new flags
		$word{$W}->{$F} = shift;
		# Update the Tied hash
		$t_word{$W} = $word{$W};
	}
	return $word{$W}->{$F};
}		

# The part of speech methods call the part_of_speech function with
# an argument indicating which flag bank to set/read. 

sub noun {		# 1.01
	return part_of_speech ("Noun_Flags",@_);
}

sub verb {		# 1.01
	return part_of_speech ("Verb_Flags",@_);
}

sub adjective {		# 1.01
	return part_of_speech ("Adjective_Flags",@_);
}

sub adverb {		# 1.01
	return part_of_speech ("Adverb_Flags",@_);
}

sub modal {		# 1.01
	return part_of_speech ("Modal_Flags",@_);
}

sub pronoun {		# 1.01
	return part_of_speech ("Pronoun_Flags",@_);
}

sub preposition {		# 1.01
	return part_of_speech ("Preposition_Flags",@_);
}

sub determiner {		# 1.01
	return part_of_speech ("Determiner_Flags",@_);
}

sub conjunction {		# 1.01
	return part_of_speech ("Conjunction_Flags",@_);
}

############ Part of Speech Frequency Section ##########################
#
# The following routines are use to get/set the frequency for the word
# when used as the requested part of speech.  By sending in a number as
# an argument, the frequency value can be updated.  In any case, the final
# frequency is returned as an integer.
#
########################################################################

# Basic frame for getting the frequency by POS type.  Called by other
# frequency functions, like noun_freq. Not intended to be called 
# directly itself.

sub frequency {					# 1.01
	my $F = shift;			# get the name of the frequency field
	my $self = shift;		# got passed on from calling method
	my $W = $self->{Text};	# Get the text of the word

	if (@_) {				# Are we setting the frequency?
		# Get the new frequency
		$word{$W}->{$F} = shift;
		# Update the Tied hash
		$t_word{$W} = $word{$W};
	}
	return $word{$W}->{$F};	# return the frequency
}		

# The frequency methods call the frequency function with
# an argument indicating which frequency to set/read. 

sub noun_freq {		# 1.01
	return frequency ("Noun_Freq",@_);
}

sub verb_freq {		# 1.01
	return frequency ("Verb_Freq",@_);
}

sub adjective_freq {		# 1.01
	return frequency ("Adjective_Freq",@_);
}

sub adverb_freq {		# 1.01
	return frequency ("Adverb_Freq",@_);
}

sub modal_freq {		# 1.01
	return frequency ("Modal_Freq",@_);
}

sub pronoun_freq {		# 1.01
	return frequency ("Pronoun_Freq",@_);
}

sub preposition_freq {		# 1.01
	return frequency ("Preposition_Freq",@_);
}

sub determiner_freq {		# 1.01
	return frequency ("Determiner_Freq",@_);
}

sub conjunction_freq {		# 1.01
	return frequency ("Conjunction_Freq",@_);
}


################ Flag Methods Section #################################
#
# The following methods are used to get/set flags in the flag banks 
# for the various parts of speech.  
#
#######################################################################

#
# Basic frame to get/set indivitual flags in the flag banks for the 
# various parts of speech.  Not intended to be called 
# directly
#

sub flags {					# 1.01
	my $FB = shift;			# get the name of the flag bank
	my $F = shift;			# the flag
	my $self = shift;		# got passed on from calling method
	my $W = $self->{Text};	# Get the text of the word

	if (@_) {				# Are we setting a flag?
		my $S = shift;	# Get the setting: 0 or 1
		if (!exists $word{$W}->{$FB}) { $word{$W}->{$FB} = 0 }
		$word{$W}->{$FB} = $S ?
			$word{$W}->{$FB} | $F :	# turn on flag
			$word{$W}->{$FB} & ~$F; # turn off flag
		$t_word{$W} = $word{$W};	# update Tied hash
	}
	return $word{$W}->{$FB} & $F;	# return setting of flag
}		


########################## Conjunction Flags ########################

sub coord_conjunction { 	# 1.01
	return flags("Conjunction_Flags",$C_COORD,@_);
}

sub subord_conjunction {	# 1.01
	return flags("Conjunction_Flags",$C_SUBORD,@_);
}

########################## Preposition Flags ########################

sub adverb_preposition {	# 1.01
	return flags("Preposition_Flags",$PR_ADVERB,@_);
}


########################## Adjective Flags ########################

#
# Adjectives and adverbs require a little extra work because
# of a unique three way flag setting.  When  one is turned on,
# the other two should be off.
#

sub comparative_adjective {		# 1.01
	if (@_ == 2) {  		# Peek ahead to argument and
						# make adjustments as needed
		my $self = $_[0];
		my $S = $_[1];	# get the setting: 0 or 1
		if ($S) {
			# if setting comparative, turn off 
			# superaltive and base flags with call to flags
			flags("Adjective_Flags",$A_SUPERLATIVE,$self,0);
			flags("Adjective_Flags",$A_BASE,$self,0);
		} 
		else {
			# if clearing comparative, turn off superlative
			# and set base by default 
			flags("Adjective_Flags",$A_SUPERLATIVE,$self,0);
			flags("Adjective_Flags",$A_BASE,$self,1);
		}
	}
	# this will adjust the comparative flag as needed
	return flags("Adjective_Flags",$A_COMPARATIVE,@_);
}

sub superlative_adjective {		# 1.01
	if (@_ == 2) {  		# Peek ahead to argument and
					# make adjustments as needed
		my $self = $_[0];
		my $S = $_[1];	# get the setting: 0 or 1
		if ($S) {
			# if setting superlative, turn off 
			# comparative and base flags with call to flags
			flags("Adjective_Flags",$A_COMPARATIVE,$self,0);
			flags("Adjective_Flags",$A_BASE,$self,0);
		} 
		else {
			# if clearing superlative, turn off comparative
			# and set base by default 
			flags("Adjective_Flags",$A_COMPARATIVE,$self,0);
			flags("Adjective_Flags",$A_BASE,$self,1);
		}
	}
	# this will adjust the comparative flag as needed
	return flags("Adjective_Flags",$A_SUPERLATIVE,@_);
}

sub base_adjective {			# 1.01
	if (@_ == 2) {  		# Peek ahead to argument and
					# make adjustments as needed
		my $self = $_[0];
		
		# base flag cannot be turned off since it is the
		# default.  If any arg is sent, base flag will be turned
		# on, comparative and superlative flags will be 
		# turned  off. 
		flags("Adjective_Flags",$A_COMPARATIVE,$self,0);
		flags("Adjective_Flags",$A_SUPERLATIVE,$self,0);
		return flags("Adjective_Flags",$A_BASE,$self,1);
	}

	# no argument was sent
	return flags("Adjective_Flags",$A_BASE,@_);
}

########################## Adverb Flags ########################

#
# Adjectives and adverbs require a little extra work because
# of a unique three way flag setting.  When  one is turned on,
# the other two should be off.
#

sub comparative_adverb {		# 1.01
	if (@_ == 2) {  		# Peek ahead to argument and
					# make adjustments as needed
		my $self = $_[0];
		my $S = $_[1];	# get the setting: 0 or 1
		if ($S) {
			# if setting comparative, turn off 
			# superaltive and base flags with call to flags
			flags("Adverb_Flags",$A_SUPERLATIVE,$self,0);
			flags("Adverb_Flags",$A_BASE,$self,0);
		} 
		else {
			# if clearing comparative, turn off superlative
			# and set base by default 
			flags("Adverb_Flags",$A_SUPERLATIVE,$self,0);
			flags("Adverb_Flags",$A_BASE,$self,1);
		}
	}
	# this will adjust the comparative flag as needed
	return flags("Adverb_Flags",$A_COMPARATIVE,@_);
}

sub superlative_adverb {		# 1.01
	if (@_ == 2) {  		# Peek ahead to argument and
					# make adjustments as needed
		my $self = $_[0];
		my $S = $_[1];	# get the setting: 0 or 1
		if ($S) {
			# if setting superlative, turn off 
			# comparative and base flags with call to flags
			flags("Adverb_Flags",$A_COMPARATIVE,$self,0);
			flags("Adverb_Flags",$A_BASE,$self,0);
		} 
		else {
			# if clearing superlative, turn off comparative
			# and set base by default 
			flags("Adverb_Flags",$A_COMPARATIVE,$self,0);
			flags("Adverb_Flags",$A_BASE,$self,1);
		}
	}
	# this will adjust the comparative flag as needed
	return flags("Adverb_Flags",$A_SUPERLATIVE,@_);
}

sub base_adverb {				# 1.01
	if (@_ == 2) {  		# Peek ahead to argument and
					# make adjustments as needed
		my $self = $_[0];
		
		# base flag cannot be turned off since it is the
		# default.  If any arg is sent, base flag will be turned
		# on, comparative and superlative flags will be 
		# turned  off. 
		flags("Adverb_Flags",$A_COMPARATIVE,$self,0);
		flags("Adverb_Flags",$A_SUPERLATIVE,$self,0);
		return flags("Adverb_Flags",$A_BASE,$self,1);
	}

	# no argument was sent
	return flags("Adverb_Flags",$A_BASE,@_);
}

########################## Determiner Flags ########################

sub singular_determiner {	# 1.01
	return flags("Determiner_Flags",$D_SINGULAR,@_);
}

sub plural_determiner {	# 1.01
	return flags("Determiner_Flags",$D_PLURAL,@_);
}

sub standalone_determiner {	# 1.01
	return flags("Determiner_Flags",$D_STANDALONE,@_);
}

sub question_determiner {	# 1.01
	return flags("Determiner_Flags",$D_QUESTION,@_);
}

########################## Pronoun Flags ########################

sub first_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_FIRST,@_);
}

sub second_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_SECOND,@_);
}

sub third_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_THIRD,@_);
}

sub singular_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_SINGULAR,@_);
}

sub plural_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_PLURAL,@_);
}

sub masculine_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_MASCULINE,@_);
}

sub feminine_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_FEMININE,@_);
}

sub neuter_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_NEUTER,@_);
}

sub nominative_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_NOMINATIVE,@_);
}

sub accusative_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_ACCUSATIVE,@_);
}

sub genitive_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_GENITIVE,@_);
}

sub person_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_PERSON,@_);
}

sub place_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_PLACE,@_);
}

sub thing_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_THING,@_);
}

sub reflexive_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_REFLEXIVE,@_);
}

sub question_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_QUESTION,@_);
}

sub adjectival_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_ADJECTIVAL,@_);
}

sub standalone_pronoun {	# 1.01
	return flags("Pronoun_Flags",$PN_STANDALONE,@_);
}

########################## Noun Flags ########################

sub singular_noun {			# 1.01
	return flags("Noun_Flags",$N_SINGULAR,@_);
}

sub plural_noun {			# 1.01
	return flags("Noun_Flags",$N_PLURAL,@_);
}

sub numberless_noun {		# 1.01
	return flags("Noun_Flags",$N_NUMBERLESS,@_);
}

sub proper_noun {			# 1.01
	return flags("Noun_Flags",$N_PROPER,@_);
}

sub masculine_noun {		# 1.01
	return flags("Noun_Flags",$N_MASCULINE,@_);
}

sub feminine_noun {			# 1.01
	return flags("Noun_Flags",$N_FEMININE,@_);
}

sub neuter_noun {			# 1.01
	return flags("Noun_Flags",$N_NEUTER,@_);
}

sub starter_noun {			# 1.01
	return flags("Noun_Flags",$N_STARTER,@_);
}

########################## Verb Flags ########################

sub infinitive_verb {		# 1.01
	return flags("Verb_Flags",$V_INFINITIVE,@_);
}

sub past_verb {				# 1.01
	return flags("Verb_Flags",$V_PAST,@_);
}

sub participle_verb {		# 1.01
	return flags("Verb_Flags",$V_PARTICIPLE,@_);
}

sub third_verb {			# 1.01
	return flags("Verb_Flags",$V_THIRD,@_);
}

sub gerund_verb {			# 1.01
	return flags("Verb_Flags",$V_GERUND,@_);
}

sub present_verb {			# 1.01
	return flags("Verb_Flags",$V_PRESENT,@_);
}

sub starter_verb {			# 1.01
	return flags("Verb_Flags",$V_STARTER,@_);
}

################## Dictionary Section ##################################
#
# Dictionary entries returned for Noun (singular form), Verb (infinitive)
# Adjectives and Adverbs (base, not comparative or superlative)
# Used where dictionary form may be different from the word's appearance
# in the sentence.
#
########################################################################

#
# Basic frame to get/set dicitonary forms for the Nouns, Verbs,
# Adjectives and Adverbs, not meant to be called directly 
#

sub dictionary {				# 1.01
	my $Dictionary = shift;	# Get Noun_Dictionary, etc.
	my $self = shift;
	my $W = $self->{Text};

	# update record if arg passed in
	if (@_) {
		$word{$W}->{$Dictionary} = shift;
		$t_word{$W} = $word{$W};	# save to TIE file
	}
	return $word{$W}->{$Dictionary};
}		

sub noun_dictionary {			# 1.01
	return dictionary("Noun_Dictionary",@_);
}

sub verb_dictionary {			# 1.01
	return dictionary("Verb_Dictionary",@_);
}

sub adjective_dictionary {		# 1.01
	return dictionary("Adjective_Dictionary",@_);
}

sub adverb_dictionary {			# 1.01
	return dictionary("Adverb_Dictionary",@_);
}

################## Persons Section #####################################
#
# Routines designed to indicate what persons (1st singular, 3rd plural,
# etc) a given noun, pronoun or verb is indicating.  Returns the 
# possibilities in the form of a common set of integer flags that can be
# 'anded' together to see where possible subject/verb agreement
# exists.
#
########################################################################

sub noun_persons {		# 1.01
	my $self = shift;

	return $P_3PS | $P_3PP if $self->numberless_noun();
	return $P_3PS if $self->singular_noun() || $self->proper_noun();
	return $P_3PP if $self->plural_noun();
}

sub pronoun_persons {	# 1.01
	my $self = shift;

	# if pronoun can be used in singular or plural
	if ($self->plural_pronoun() && $self->singular_pronoun()) {
		return $P_1PS if $self->first_pronoun();
		return $P_2PS if $self->second_pronoun();
		return $P_3PS if $self->third_pronoun();
	} 
	elsif ($self->singular_pronoun()) {
		return $P_1PS if $self->first_pronoun();
		return $P_2PS if $self->second_pronoun();
		return $P_3PS if $self->third_pronoun();
	} 
	elsif ($self->plural_pronoun()) {
		return $P_1PP if $self->first_pronoun();
		return $P_2PP if $self->second_pronoun();
		return $P_3PP if $self->third_pronoun();
	} 
	else { return 0 }
}

sub verb_persons {		# 1.01
	my $self = shift;
	my $W = $self->{Text};

	return 0 if !$self->verb();		# Not a verb anyway
	return $P_1PS if $W eq "am";	# get the wierd 'be' stuff done
	return $P_2PS | $P_2PP | $P_1PP | $P_3PP
		if $W eq "are";
	return $P_3PS if $W eq "is";
	return $P_1PS | $P_3PS if $W eq "was";
	return $P_2PS | $P_2PP | $P_1PP | $P_3PP if $W eq "were";
	
	# present aside from 3rd singular
	return $P_ALL & ~$P_3PS if $self->present_verb();

	# third singular
	return $P_3PS if $self->third_verb();

	# past tense
	return $P_ALL if $self->past_verb();
}

###################### Support Routines Section #########################
#
# Routines for loading and support the databases
#
#########################################################################

#
# Called with first instance of a word to set up the
# tied hashes.
#
sub tie_hash {		# 1.01

	no strict;
	tie %t_word,'MLDBM',"word.db", O_CREAT|O_RDWR,0666 || 
		die "could not tie word.db\n";
	use strict;

	$hash_tied = 1;	# let future objects know hash is tied 
}

#
# The following four functions are used for importing/exporting 
# between a binary hash file and a text data file
#
 
sub bin2dec {		# 1.01
	return unpack("N", pack("B32", substr("0" x 32 . shift, -32)));
}

sub dec2bin {		# 1.01
	return unpack("B32", pack("N",shift));
}

#
# Import words from text database, word.txt, into binary, word.db
#
sub import_word {	# 1.01
	my @F;
	my $L;
	my $C;
	my $K;

	&tie_hash if (!$hash_tied);

	keys (%t_word) = 100000;

	print "Loading word.db from word.txt\n";
	open FILE,"word.txt" or die "Could not open file word.txt\n";
	$C = 0;
	while ($L = <FILE>) {
		chomp $L;

		@F = split(/,/,$L);
		$t_word{$F[0]} = {	"Text" => $F[0],
					"Noun_Dictionary" => $F[1],
					"Verb_Dictionary" => $F[2],
					"Adjective_Dictionary" => $F[3],
					"Adverb_Dictionary" => $F[4],
					"Noun_Flags" => bin2dec($F[5]),
					"Verb_Flags" => bin2dec($F[6]),
					"Adjective_Flags" => bin2dec($F[7]),
					"Adverb_Flags" => bin2dec($F[8]),
					"Modal_Flags" => bin2dec($F[9]),
					"Pronoun_Flags" => bin2dec($F[10]),
					"Preposition_Flags" => bin2dec($F[11]),
					"Determiner_Flags" => bin2dec($F[12]),
					"Conjunction_Flags" => bin2dec($F[13]),
					"Noun_Freq" => $F[14],
					"Verb_Freq" => $F[15],
					"Adjective_Freq" => $F[16],
					"Adverb_Freq" => $F[17],
					"Modal_Freq" => $F[18],
					"Pronoun_Freq" => $F[19],
					"Preposition_Freq" => $F[20],
					"Determiner_Freq" => $F[21],
					"Conjunction_Freq" => $F[22]
		};
		$C++;
		if (!($C % 200)) { print "$C records loaded\n" }
	}
	close FILE;

} ### end of import_word

#
# Export words to text database, word.txt, from binary, word.db
#
sub export_word {	# 1.01
	my $K;
	my $C;
	my $L;

	&tie_hash if (!$hash_tied);
	open OUT,"> word.txt";

	# load the hash file into ram
	foreach $K (keys %t_word) { $word{$K} = $t_word{$K} }

	# dump Ram copy to file
	foreach $K (sort keys %word) {
		$L = $word{$K}->{Text};
		$L .= ",";
		if (exists $word{$K}->{Noun_Dictionary}){
			$L .= $word{$K}->{Noun_Dictionary}; 
		}
		$L .= ",";
		if (exists $word{$K}->{Verb_Dictionary}){
			$L .= $word{$K}->{Verb_Dictionary}; 
		}
		$L .= ",";
		if (exists $word{$K}->{Adjective_Dictionary}){
			$L .= $word{$K}->{Adjective_Dictionary}; 
		}
		$L .= ",";
		if (exists $word{$K}->{Adverb_Dictionary}){
			$L .= $word{$K}->{Adverb_Dictionary}; 
		}
		$L .= ",";
		if ($word{$K}->{Noun_Flags}){
			$L .= dec2bin($word{$K}->{Noun_Flags});
		} 
		$L .= ",";
		if ($word{$K}->{Verb_Flags}){
			$L .= dec2bin($word{$K}->{Verb_Flags}); 
		}
		$L .= ",";
		if ($word{$K}->{Adjective_Flags}){
			$L .= dec2bin($word{$K}->{Adjective_Flags}); 
		}
		$L .= ",";
		if ($word{$K}->{Adverb_Flags}){
			$L .= dec2bin($word{$K}->{Adverb_Flags});
		} 
		$L .= ",";
		if ($word{$K}->{Modal_Flags}){
			$L .= dec2bin($word{$K}->{Modal_Flags});
		} 
		$L .= ",";
		if ($word{$K}->{Pronoun_Flags}){
			$L .= dec2bin($word{$K}->{Pronoun_Flags});
		} 
		$L .= ",";
		if ($word{$K}->{Preposition_Flags}){
			$L .= dec2bin($word{$K}->{Preposition_Flags});
		} 
		$L .= ",";
		if ($word{$K}->{Determiner_Flags}){
			$L .= dec2bin($word{$K}->{Determiner_Flags});
		}
		$L .= ",";
		if ($word{$K}->{Conjunction_Flags}){
			$L .= dec2bin($word{$K}->{Conjunction_Flags});
		} 
		$L .= ",";
		if ($word{$K}->{Noun_Freq}){
			$L .= $word{$K}->{Noun_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Verb_Freq}){
			$L .= $word{$K}->{Verb_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Adjective_Freq}){
			$L .= $word{$K}->{Adjective_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Adverb_Freq}){
			$L .= $word{$K}->{Adverb_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Modal_Freq}){
			$L .= $word{$K}->{Modal_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Pronoun_Freq}){
			$L .= $word{$K}->{Pronoun_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Preposition_Freq}){
			$L .= $word{$K}->{Preposition_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Determiner_Freq}){
			$L .= $word{$K}->{Determiner_Freq}; 
		}
		$L .= ",";
		if ($word{$K}->{Conjunction_Freq}){
			$L .= $word{$K}->{Conjunction_Freq}; 
		}
		$C++;

		print OUT "$L\n";
	}

	close OUT;

	print "$C records exported\n";

} ### end of export_word


1;
__END__

=head1 NAME

Harvey::Word - Perl extension for creating word objects

=head1 SYNOPSIS

  use Harvey::Word;
  my $W = Word->new("grape");

    Word object module for Harvey.  Looks up information on
    a word for all forms and gives information to calling objects.


=head1 DESCRIPTION

  The purpose of the Word module is to create Word objects that can be 
  queried for syntactic information about the word.

    Version 1.01, words can be queried for their dictionary form, part 
    of speech, many attributes on the basis of the part of speech, 
    frequency, what persons are possible, i.e. 1st singular, 3rd plural, 
    etc., and the likeliest parts of speech that the word could be based 
    on the frequencies in the BNC.

    Most methods which return a characteristic of the word can also be 
    used to turn on or off the characteristic by passing a 0 or 1 for
    boolean flags, text for text queries and numbers for the frequency.

    The data is used from a TIE hash database, but can be exported/imported
    from the ASCII file word.txt using the export_word and import_word 
    functions.  

  The following methods are supported:

    new: 	Constructor.
    text: 	Get the text of the word.
    load_tie:	Load a word record from the TIE hash (%t_word) into the 
    		memory hash (%word). Done automatically from the constructor.
    prioritize: Returns an ordered array of the most likely parts of speech
		for a given word based on the BNC frequency counts.
    noun:	Retuns the noun flags if the word is a noun, otherwise 0.  Can
		be used as a boolean test for whether the word can be a noun, 
		but also can set or retieve the noun flags for a word, which are
		stored as bytes in an integer.  To set the flags, send in an 
		integer as the argument.  	
    verb:	Same as noun, but for verbs.
    adjective:	Same as noun, but for adjectives.
    adverb:	Same as noun, but for adverbs.
    modal:	Same as noun, but for modals.
    pronoun:	Same as noun, but for pronouns.
    preposition:	Same as noun, but for prepositions.
    determiner:	Same as noun, but for determiners.
    conjunction:	Same as noun, but for conjunctions.
    noun_freq:	Gets/set the noun frequency.
    verb_freq:	Gets/set the verb frequency.
    adjective_freq:	Gets/set the adjective frequency.
    adverb_freq:	Gets/set the adverb frequency.
    modal_freq:	Gets/set the modal frequency.
    pronoun_freq:	Gets/set the pronoun frequency.
    preposition_freq:	Gets/set the preposition frequency.
    determiner_freq:	Gets/set the determiner frequency.
    conjunction_freq:	Gets/set the conjunction frequency.
    coord_conjunction: 	Gets/set coordinating flag for conjunctions.
    subord_conjunction: 	Gets/set subordinating flag for conjunctions.
    adverb_preposition:	Gets/set whether preposition can be used 
    			alone as adverb.
    base_adjective:	Gets/set base flag for adjectives.
    comparative_adjective:	Gets/set comparative flag for adjectives.
    superlative_adjective:	Gets/set superlative flag for adjectives.
    base_adverb:	Gets/set base adverb flag.
    comparative_adverb:	Gets/set comparative adverb flag.
    superlative_adverb:	Gets/set superlative adverb flag.
    singular determiner: 	Gets/set singular flag for determiners.
    plural_determiner:	Get/set plural flag for determiners.
    standalone_determiner:	Get/set standalone flag for determiners.
    question_determiner:	Get/set question flag for determiners.
    first_pronoun:	Get/set first person flag for pronouns.
    second_pronoun:	Get/set second person flag for pronouns.
    third_pronoun:	Get/set third person flag for pronouns.
    singular_pronoun:	Get/set singular flag for pronouns.
    plural_pronoun:	Get/set plural flag for pronouns.
    masculine_pronoun:	Get/set masculine flag for pronouns.
    feminine_pronoun:	Get/set feminine flag for pronouns.
    neuter_pronoun:	Get/set neuter flag for pronouns.
    nominative_pronoun:	Get/set nominative flag for pronouns.
    accusativey_pronoun:	Get/set accusative flag for pronouns.
    genitive_pronoun:	Get/set genitive flag for pronouns.
    person_pronoun:	Get/set person flag for pronouns.
    place_pronoun:	Get/set place flag for pronouns.
    thing_pronoun:	Get/set thing flag for pronouns.
    reflexive_pronoun:	Get/set reflexive flag for pronouns.
    question_pronoun:	Get/set question flag for pronouns.
    adjectival_pronoun:	Get/set adjectival flag for pronouns: our your
    standalone_pronoun:	Get/set standalone flag for pronouns: ours yours
    singular_noun:	Get/set singular flag for nouns.
    plural_noun:	Get/set plural flag for nouns.
    numberless_noun:	Get/set numberless flag for nouns.
    proper_noun:	Get/set proper flag for nouns.
    masculine_noun:	Get/set masculine flag for nouns.
    feminine_noun:	Get/set feminine flag for nouns.
    neuter_noun:	Get/set neuter flag for nouns.
    starter_noun:	Get/set starter flag for nouns.
    infinitive_verb:	Get/set infinitive flag for verbs.
    past_verb:	Get/set infinitive flag for verbs.
    participle_verb:	Get/set infinitive flag for verbs.
    third_verb:	Get/set infinitive flag for verbs.
    gerund_verb:	Get/set infinitive flag for verbs.
    present_verb:	Get/set infinitive flag for verbs.
    starter_verb:	Get/set infinitive flag for verbs.
    noun_dictionary:	Get/set dictionary form for a noun, ie. singular.
    verb_dictionary:	Get/set dictionary form for a verb, ie. infinitive
    adjective_dictionary:	Get/set dictionary form for a adjective i.e. base
    adverb_dictionary:	Get/set dictionary form for a adverb, i.e. base form
    noun_persons:	Returns the possible persons for a noun.  The 'persons'
			functions return an Integer with the possible persons
			(i.e. first singular, second plural, etc) stored as
			bits.  This makes it easy to check for Subject verb
			agreement or pronoun/noun agreement by 'anding' the
			flags together.
    pronoun_persons:	Returns the possible persons for a pronoun.
    verb_persons:	Returns the possible persons for a verb.
    tie_hash:	Ties the hash, %t_word to the file dic/word.db.	
    import_word:	Builds the hash TIE file 'dic/word.db' from
			the text file 'word.txt'.
    export_word:	Exports data from the TIE file 'dic/word.db' 
			to the text file 'word.txt'.


=head2 EXPORT

None by default.


=head1 AUTHOR

Chris Meyer, E<lt>chris@mytechs.comE<gt>

=head1 COPYWRITE
  Copywrite (c) 2002, Chris Meyer.  All rights reserved.  This is 
  free software and can be used under the same terms as Perl itself.

=head1 VERSION 

  1.01

=head1 RELATED LIBRARIES

  My heartfelt thanks to Adam Kilgarriff for his work on the BNC 
  (British National Corpus) which forms the basis for the word.db.
  I have added and massaged it a bit, but I would never have gotten
  this far without it.  The BNC can be visited at
  http://www.itri.brighton.ac.uc/~Adam.Kilgarriff/bnc-readme.html.

L<perl>.

=cut
