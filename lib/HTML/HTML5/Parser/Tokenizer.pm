package HTML::HTML5::Parser::Tokenizer; # -*- Perl -*-
use strict;
our $VERSION='0.01';

BEGIN {
  require Exporter;
  push our @ISA, 'Exporter';

  our @EXPORT_OK = qw(
    DOCTYPE_TOKEN
    COMMENT_TOKEN
    START_TAG_TOKEN
    END_TAG_TOKEN
    END_OF_FILE_TOKEN
    CHARACTER_TOKEN
    PI_TOKEN
    ABORT_TOKEN
    END_OF_DOCTYPE_TOKEN
    ATTLIST_TOKEN
    ELEMENT_TOKEN
    GENERAL_ENTITY_TOKEN
    PARAMETER_ENTITY_TOKEN
    NOTATION_TOKEN
  );
  
  our %EXPORT_TAGS = (
    token => [qw(
      DOCTYPE_TOKEN
      COMMENT_TOKEN
      START_TAG_TOKEN
      END_TAG_TOKEN
      END_OF_FILE_TOKEN
      CHARACTER_TOKEN
      PI_TOKEN
      ABORT_TOKEN
      END_OF_DOCTYPE_TOKEN
      ATTLIST_TOKEN
      ELEMENT_TOKEN
      GENERAL_ENTITY_TOKEN
      PARAMETER_ENTITY_TOKEN
      NOTATION_TOKEN
    )],
  );
}

## NOTE: Differences from the XML5 draft are marked as "XML5:".

## Token types

sub DOCTYPE_TOKEN () { 1 } ## XML5: No DOCTYPE token.
sub COMMENT_TOKEN () { 2 }
sub START_TAG_TOKEN () { 3 }
sub END_TAG_TOKEN () { 4 }
sub END_OF_FILE_TOKEN () { 5 }
sub CHARACTER_TOKEN () { 6 }
sub PI_TOKEN () { 7 } ## NOTE: XML only.
sub ABORT_TOKEN () { 8 } ## NOTE: For internal processing.
sub END_OF_DOCTYPE_TOKEN () { 9 } ## NOTE: XML only.
sub ATTLIST_TOKEN () { 10 } ## NOTE: XML only.
sub ELEMENT_TOKEN () { 11 } ## NOTE: XML only.
sub GENERAL_ENTITY_TOKEN () { 12 } ## NOTE: XML only.
sub PARAMETER_ENTITY_TOKEN () { 13 } ## NOTE: XML only.
sub NOTATION_TOKEN () { 14 } ## NOTE: XML only.

## XML5: XML5 has "empty tag token".  In this implementation, it is
## represented as a start tag token with $self->{self_closing} flag
## set to true.

## XML5: XML5 has "short end tag token".  In this implementation, it
## is represented as an end tag token with $token->{tag_name} flag set
## to an empty string.

package HTML::HTML5::Parser::TagSoupParser;

BEGIN { HTML::HTML5::Parser::Tokenizer->import (':token') }

## Content model flags

sub CM_ENTITY () { 0b001 } # & markup in data
sub CM_LIMITED_MARKUP () { 0b010 } # < markup in data (limited)
sub CM_FULL_MARKUP () { 0b100 } # < markup in data (any)

sub PLAINTEXT_CONTENT_MODEL () { 0 }
sub CDATA_CONTENT_MODEL () { CM_LIMITED_MARKUP }
sub RCDATA_CONTENT_MODEL () { CM_ENTITY | CM_LIMITED_MARKUP }
sub PCDATA_CONTENT_MODEL () { CM_ENTITY | CM_FULL_MARKUP }

## Tokenizer states

sub DATA_STATE () { 0 }
#sub ENTITY_DATA_STATE () { 1 }
sub TAG_OPEN_STATE () { 2 }
sub CLOSE_TAG_OPEN_STATE () { 3 }
sub TAG_NAME_STATE () { 4 }
sub BEFORE_ATTRIBUTE_NAME_STATE () { 5 }
sub ATTRIBUTE_NAME_STATE () { 6 }
sub AFTER_ATTRIBUTE_NAME_STATE () { 7 }
sub BEFORE_ATTRIBUTE_VALUE_STATE () { 8 }
sub ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE () { 9 }
sub ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE () { 10 }
sub ATTRIBUTE_VALUE_UNQUOTED_STATE () { 11 }
#sub ENTITY_IN_ATTRIBUTE_VALUE_STATE () { 12 }
sub MARKUP_DECLARATION_OPEN_STATE () { 13 }
sub COMMENT_START_STATE () { 14 }
sub COMMENT_START_DASH_STATE () { 15 }
sub COMMENT_STATE () { 16 }
sub COMMENT_END_STATE () { 17 }
sub COMMENT_END_BANG_STATE () { 102 }
sub COMMENT_END_SPACE_STATE () { 103 } ## LAST
sub COMMENT_END_DASH_STATE () { 18 }
sub BOGUS_COMMENT_STATE () { 19 }
sub DOCTYPE_STATE () { 20 }
sub BEFORE_DOCTYPE_NAME_STATE () { 21 }
sub DOCTYPE_NAME_STATE () { 22 }
sub AFTER_DOCTYPE_NAME_STATE () { 23 }
sub BEFORE_DOCTYPE_PUBLIC_IDENTIFIER_STATE () { 24 }
sub DOCTYPE_PUBLIC_IDENTIFIER_DOUBLE_QUOTED_STATE () { 25 }
sub DOCTYPE_PUBLIC_IDENTIFIER_SINGLE_QUOTED_STATE () { 26 }
sub AFTER_DOCTYPE_PUBLIC_IDENTIFIER_STATE () { 27 }
sub BEFORE_DOCTYPE_SYSTEM_IDENTIFIER_STATE () { 28 }
sub DOCTYPE_SYSTEM_IDENTIFIER_DOUBLE_QUOTED_STATE () { 29 }
sub DOCTYPE_SYSTEM_IDENTIFIER_SINGLE_QUOTED_STATE () { 30 }
sub AFTER_DOCTYPE_SYSTEM_IDENTIFIER_STATE () { 31 }
sub BOGUS_DOCTYPE_STATE () { 32 }
sub AFTER_ATTRIBUTE_VALUE_QUOTED_STATE () { 33 }
sub SELF_CLOSING_START_TAG_STATE () { 34 }
sub CDATA_SECTION_STATE () { 35 }
sub MD_HYPHEN_STATE () { 36 } # "markup declaration open state" in the spec
sub MD_DOCTYPE_STATE () { 37 } # "markup declaration open state" in the spec
sub MD_CDATA_STATE () { 38 } # "markup declaration open state" in the spec
sub CDATA_RCDATA_CLOSE_TAG_STATE () { 39 } # "close tag open state" in the spec
sub CDATA_SECTION_MSE1_STATE () { 40 } # "CDATA section state" in the spec
sub CDATA_SECTION_MSE2_STATE () { 41 } # "CDATA section state" in the spec
sub PUBLIC_STATE () { 42 } # "after DOCTYPE name state" in the spec
sub SYSTEM_STATE () { 43 } # "after DOCTYPE name state" in the spec
## NOTE: "Entity data state", "entity in attribute value state", and
## "consume a character reference" algorithm are jointly implemented
## using the following six states:
sub ENTITY_STATE () { 44 }
sub ENTITY_HASH_STATE () { 45 }
sub NCR_NUM_STATE () { 46 }
sub HEXREF_X_STATE () { 47 }
sub HEXREF_HEX_STATE () { 48 }
sub ENTITY_NAME_STATE () { 49 }
sub PCDATA_STATE () { 50 } # "data state" in the spec

## XML-only states
sub PI_STATE () { 51 }
sub PI_TARGET_STATE () { 52 }
sub PI_TARGET_AFTER_STATE () { 53 }
sub PI_DATA_STATE () { 54 }
sub PI_AFTER_STATE () { 55 }
sub PI_DATA_AFTER_STATE () { 56 }
sub DOCTYPE_INTERNAL_SUBSET_STATE () { 57 }
sub DOCTYPE_INTERNAL_SUBSET_AFTER_STATE () { 58 }
sub BOGUS_DOCTYPE_INTERNAL_SUBSET_AFTER_STATE () { 59 }
sub DOCTYPE_TAG_STATE () { 60 }
sub DOCTYPE_MARKUP_DECLARATION_OPEN_STATE () { 61 }
sub MD_ATTLIST_STATE () { 62 }
sub MD_E_STATE () { 63 }
sub MD_ELEMENT_STATE () { 64 }
sub MD_ENTITY_STATE () { 65 }
sub MD_NOTATION_STATE () { 66 }
sub DOCTYPE_MD_STATE () { 67 }
sub BEFORE_MD_NAME_STATE () { 68 }
sub MD_NAME_STATE () { 69 }
sub DOCTYPE_ENTITY_PARAMETER_BEFORE_STATE () { 70 }
sub DOCTYPE_ATTLIST_NAME_AFTER_STATE () { 71 }
sub DOCTYPE_ATTLIST_ATTRIBUTE_NAME_STATE () { 72 }
sub DOCTYPE_ATTLIST_ATTRIBUTE_NAME_AFTER_STATE () { 73 }
sub DOCTYPE_ATTLIST_ATTRIBUTE_TYPE_STATE () { 74 }
sub DOCTYPE_ATTLIST_ATTRIBUTE_TYPE_AFTER_STATE () { 75 }
sub BEFORE_ALLOWED_TOKEN_STATE () { 76 }
sub ALLOWED_TOKEN_STATE () { 77 }
sub AFTER_ALLOWED_TOKEN_STATE () { 78 }
sub AFTER_ALLOWED_TOKENS_STATE () { 79 }
sub BEFORE_ATTR_DEFAULT_STATE () { 80 }
sub DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_BEFORE_STATE () { 81 }
sub DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_STATE () { 82 }
sub DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_AFTER_STATE () { 83 }
sub AFTER_ATTLIST_ATTR_VALUE_QUOTED_STATE () { 84 }
sub BEFORE_NDATA_STATE () { 85 }
sub NDATA_STATE () { 86 }
sub AFTER_NDATA_STATE () { 87 }
sub BEFORE_NOTATION_NAME_STATE () { 88 }
sub NOTATION_NAME_STATE () { 89 }
sub DOCTYPE_ENTITY_VALUE_DOUBLE_QUOTED_STATE () { 90 }
sub DOCTYPE_ENTITY_VALUE_SINGLE_QUOTED_STATE () { 91 }
sub ENTITY_VALUE_ENTITY_STATE () { 92 }
sub AFTER_ELEMENT_NAME_STATE () { 93 }
sub BEFORE_ELEMENT_CONTENT_STATE () { 94 }
sub CONTENT_KEYWORD_STATE () { 95 }
sub AFTER_CM_GROUP_OPEN_STATE () { 96 }
sub CM_ELEMENT_NAME_STATE () { 97 }
sub AFTER_CM_ELEMENT_NAME_STATE () { 98 }
sub AFTER_CM_GROUP_CLOSE_STATE () { 99 }
sub AFTER_MD_DEF_STATE () { 100 }
sub BOGUS_MD_STATE () { 101 }

## Tree constructor state constants (see HTML::HTML5::Parser for the full
## list and descriptions)

sub IN_FOREIGN_CONTENT_IM () { 0b100000000000 }
sub FOREIGN_EL () { 0b1_00000000000 }

## Character reference mappings

my $charref_map = {
  0x00 => 0xFFFD, # REPLACEMENT CHARACTER
  0x0D => 0x000A,
  0x80 => 0x20AC,
  0x81 => 0x0081,
  0x82 => 0x201A,
  0x83 => 0x0192,
  0x84 => 0x201E,
  0x85 => 0x2026,
  0x86 => 0x2020,
  0x87 => 0x2021,
  0x88 => 0x02C6,
  0x89 => 0x2030,
  0x8A => 0x0160,
  0x8B => 0x2039,
  0x8C => 0x0152,
  0x8D => 0x008D,
  0x8E => 0x017D,
  0x8F => 0x008F,
  0x90 => 0x0090,
  0x91 => 0x2018,
  0x92 => 0x2019,
  0x93 => 0x201C,
  0x94 => 0x201D,
  0x95 => 0x2022,
  0x96 => 0x2013,
  0x97 => 0x2014,
  0x98 => 0x02DC,
  0x99 => 0x2122,
  0x9A => 0x0161,
  0x9B => 0x203A,
  0x9C => 0x0153,
  0x9D => 0x009D,
  0x9E => 0x017E,
  0x9F => 0x0178,
}; # $charref_map
$charref_map->{$_} = $_
    for 0x0001..0x0008, 0x000B, 0x000E..0x001F, 0x007F, 
        0xD800..0xDFFF, 0xFDD0..0xFDEF,
        0xFFFE, 0xFFFF, 0x1FFFE, 0x1FFFF, 0x2FFFE, 0x2FFFF, 0x3FFFE, 0x3FFFF,
        0x4FFFE, 0x4FFFF, 0x5FFFE, 0x5FFFF, 0x6FFFE, 0x6FFFF, 0x7FFFE,
        0x7FFFF, 0x8FFFE, 0x8FFFF, 0x9FFFE, 0x9FFFF, 0xAFFFE, 0xAFFFF,
        0xBFFFE, 0xBFFFF, 0xCFFFE, 0xCFFFF, 0xDFFFE, 0xDFFFF, 0xEFFFE,
        0xEFFFF, 0xFFFFE, 0xFFFFF, 0x10FFFE, 0x10FFFF;

## Implementations MUST act as if state machine in the spec

sub _initialize_tokenizer ($) {
  my $self = shift;

  ## NOTE: Fields set by |new| constructor:
  #$self->{level}
  #$self->{set_nc}
  #$self->{parse_error}
  #$self->{is_xml} (if XML)

  $self->{state} = DATA_STATE; # MUST
  $self->{s_kwd} = ''; # Data state keyword
  #$self->{kwd} = ''; # State-dependent keyword; initialized when used
  #$self->{entity__value}; # initialized when used
  #$self->{entity__match}; # initialized when used
  $self->{content_model} = PCDATA_CONTENT_MODEL; # be
  undef $self->{ct}; # current token
  undef $self->{ca}; # current attribute
  undef $self->{last_stag_name}; # last emitted start tag name
  #$self->{prev_state}; # initialized when used
  delete $self->{self_closing};
  $self->{char_buffer} = '';
  $self->{char_buffer_pos} = 0;
  $self->{nc} = -1; # next input character
  #$self->{next_nc}
  
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
  $self->{token} = [];
  # $self->{escape}
} # _initialize_tokenizer

## A token has:
##   ->{type} == DOCTYPE_TOKEN, START_TAG_TOKEN, END_TAG_TOKEN, COMMENT_TOKEN,
##       CHARACTER_TOKEN, END_OF_FILE_TOKEN, PI_TOKEN, or ABORT_TOKEN
##   ->{name} (DOCTYPE_TOKEN)
##   ->{tag_name} (START_TAG_TOKEN, END_TAG_TOKEN)
##   ->{target} (PI_TOKEN)
##   ->{pubid} (DOCTYPE_TOKEN)
##   ->{sysid} (DOCTYPE_TOKEN)
##   ->{quirks} == 1 or 0 (DOCTYPE_TOKEN): "force-quirks" flag
##   ->{attributes} isa HASH (START_TAG_TOKEN, END_TAG_TOKEN)
##        ->{name}
##        ->{value}
##        ->{has_reference} == 1 or 0
##        ->{index}: Index of the attribute in a tag.
##   ->{data} (COMMENT_TOKEN, CHARACTER_TOKEN, PI_TOKEN)
##   ->{has_reference} == 1 or 0 (CHARACTER_TOKEN)
##   ->{last_index} (ELEMENT_TOKEN): Next attribute's index - 1.
##   ->{has_internal_subset} = 1 or 0 (DOCTYPE_TOKEN)

## NOTE: The "self-closing flag" is hold as |$self->{self_closing}|.
##     |->{self_closing}| is used to save the value of |$self->{self_closing}|
##     while the token is pushed back to the stack.

## Emitted token MUST immediately be handled by the tree construction state.

## Before each step, UA MAY check to see if either one of the scripts in
## "list of scripts that will execute as soon as possible" or the first
## script in the "list of scripts that will execute asynchronously",
## has completed loading.  If one has, then it MUST be executed
## and removed from the list.

## TODO: Polytheistic slash SHOULD NOT be used. (Applied only to atheists.)
## (This requirement was dropped from HTML5 spec, unfortunately.)

my $is_space = {
  0x0009 => 1, # CHARACTER TABULATION (HT)
  0x000A => 1, # LINE FEED (LF)
  #0x000B => 0, # LINE TABULATION (VT)
  0x000C => 1, # FORM FEED (FF) ## XML5: Not a space character.
  #0x000D => 1, # CARRIAGE RETURN (CR)
  0x0020 => 1, # SPACE (SP)
};

sub _get_next_token ($) {
  my $self = shift;

  if ($self->{self_closing}) {
    $self->{parse_error}->(level => $self->{level}->{must}, type => 'nestc', token => $self->{ct});
    ## NOTE: The |self_closing| flag is only set by start tag token.
    ## In addition, when a start tag token is emitted, it is always set to
    ## |ct|.
    delete $self->{self_closing};
  }

  if (@{$self->{token}}) {
    $self->{self_closing} = $self->{token}->[0]->{self_closing};
    return shift @{$self->{token}};
  }

  A: {
    if ($self->{state} == PCDATA_STATE) {
      ## NOTE: Same as |DATA_STATE|, but only for |PCDATA| content model.

      if ($self->{nc} == 0x0026) { # &
        
        ## NOTE: In the spec, the tokenizer is switched to the 
        ## "entity data state".  In this implementation, the tokenizer
        ## is switched to the |ENTITY_STATE|, which is an implementation
        ## of the "consume a character reference" algorithm.
        $self->{entity_add} = -1;
        $self->{prev_state} = DATA_STATE;
        $self->{state} = ENTITY_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003C) { # <
        
        $self->{state} = TAG_OPEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        
        return  ({type => END_OF_FILE_TOKEN,
                  line => $self->{line}, column => $self->{column}});
        last A; ## TODO: ok?
      } else {
        
        #
      }

      # Anything else
      my $token = {type => CHARACTER_TOKEN,
                   data => chr $self->{nc},
                   line => $self->{line}, column => $self->{column},
                  };
      $self->{read_until}->($token->{data}, q[<&], length $token->{data});

      ## Stay in the state.
      
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
      return  ($token);
      redo A;
    } elsif ($self->{state} == DATA_STATE) {
      $self->{s_kwd} = '' unless defined $self->{s_kwd};
      if ($self->{nc} == 0x0026) { # &
        $self->{s_kwd} = '';
	if ($self->{content_model} & CM_ENTITY and # PCDATA | RCDATA
            not $self->{escape}) {
          
          ## NOTE: In the spec, the tokenizer is switched to the 
          ## "entity data state".  In this implementation, the tokenizer
          ## is switched to the |ENTITY_STATE|, which is an implementation
          ## of the "consume a character reference" algorithm.
          $self->{entity_add} = -1;
          $self->{prev_state} = DATA_STATE;
          $self->{state} = ENTITY_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } else {
          
          #
        }
      } elsif ($self->{nc} == 0x002D) { # -
	if ($self->{content_model} & CM_LIMITED_MARKUP) { # RCDATA | CDATA
          if ($self->{s_kwd} eq '<!-') {
            
            $self->{escape} = 1; # unless $self->{escape};
            $self->{s_kwd} = '--';
            #
          } elsif ($self->{s_kwd} eq '-') {
            
            $self->{s_kwd} = '--';
            #
          } elsif ($self->{s_kwd} eq '<!' or $self->{s_kwd} eq '-') {
            
            $self->{s_kwd} .= '-';
            #
          } else {
            
            $self->{s_kwd} = '-';
            #
          }
        }
        
        #
      } elsif ($self->{nc} == 0x0021) { # !
        if (length $self->{s_kwd}) {
          
          $self->{s_kwd} .= '!';
          #
        } else {
          
          #$self->{s_kwd} = '';
          #
        }
        #
      } elsif ($self->{nc} == 0x003C) { # <
        if ($self->{content_model} & CM_FULL_MARKUP or # PCDATA
            (($self->{content_model} & CM_LIMITED_MARKUP) and # CDATA | RCDATA
             not $self->{escape})) {
          
          $self->{state} = TAG_OPEN_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } else {
          
          $self->{s_kwd} = '';
          #
        }
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{escape} and
            ($self->{content_model} & CM_LIMITED_MARKUP)) { # RCDATA | CDATA
          if ($self->{s_kwd} eq '--') {
            
            delete $self->{escape};
            #
          } else {
            
            #
          }
        } elsif ($self->{is_xml} and $self->{s_kwd} eq ']]') {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched mse', ## TODO: type
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 1);
          #
        } else {
          
          #
        }
        
        $self->{s_kwd} = '';
        #
      } elsif ($self->{nc} == 0x005D) { # ]
        if ($self->{s_kwd} eq ']' or $self->{s_kwd} eq '') {
          
          $self->{s_kwd} .= ']';
        } elsif ($self->{s_kwd} eq ']]') {
          
          #
        } else {
          
          $self->{s_kwd} = '';
        }
        #
      } elsif ($self->{nc} == -1) {
        
        $self->{s_kwd} = '';
        return  ({type => END_OF_FILE_TOKEN,
                  line => $self->{line}, column => $self->{column}});
        last A; ## TODO: ok?
      } else {
        
        $self->{s_kwd} = '';
        #
      }

      # Anything else
      my $token = {type => CHARACTER_TOKEN,
                   data => chr $self->{nc},
                   line => $self->{line}, column => $self->{column},
                  };
      if ($self->{read_until}->($token->{data}, q{-!<>&\]},
                                length $token->{data})) {
        $self->{s_kwd} = '';
      }

      ## Stay in the data state.
      if (not $self->{is_xml} and
          $self->{content_model} == PCDATA_CONTENT_MODEL) {
        
        $self->{state} = PCDATA_STATE;
      } else {
        
        ## Stay in the state.
      }
      
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
      return  ($token);
      redo A;
    } elsif ($self->{state} == TAG_OPEN_STATE) {
      ## XML5: "tag state".

      if ($self->{content_model} & CM_LIMITED_MARKUP) { # RCDATA | CDATA
        if ($self->{nc} == 0x002F) { # /
          
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          $self->{state} = CLOSE_TAG_OPEN_STATE;
          redo A;
        } elsif ($self->{nc} == 0x0021) { # !
          
          $self->{s_kwd} = $self->{escaped} ? '' : '<';
          #
        } else {
          
          $self->{s_kwd} = '';
          #
        }

        ## reconsume
        $self->{state} = DATA_STATE;
        return  ({type => CHARACTER_TOKEN, data => '<',
                  line => $self->{line_prev},
                  column => $self->{column_prev},
                 });
        redo A;
      } elsif ($self->{content_model} & CM_FULL_MARKUP) { # PCDATA
        if ($self->{nc} == 0x0021) { # !
          
          $self->{state} = MARKUP_DECLARATION_OPEN_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } elsif ($self->{nc} == 0x002F) { # /
          
          $self->{state} = CLOSE_TAG_OPEN_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } elsif (0x0041 <= $self->{nc} and
                 $self->{nc} <= 0x005A) { # A..Z
          
          $self->{ct}
            = {type => START_TAG_TOKEN,
               tag_name => chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020)),
               line => $self->{line_prev},
               column => $self->{column_prev}};
          $self->{state} = TAG_NAME_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } elsif (0x0061 <= $self->{nc} and
                 $self->{nc} <= 0x007A) { # a..z
          
          $self->{ct} = {type => START_TAG_TOKEN,
                                    tag_name => chr ($self->{nc}),
                                    line => $self->{line_prev},
                                    column => $self->{column_prev}};
          $self->{state} = TAG_NAME_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } elsif ($self->{nc} == 0x003E) { # >
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'empty start tag',
                          line => $self->{line_prev},
                          column => $self->{column_prev});
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

          return  ({type => CHARACTER_TOKEN, data => '<>',
                    line => $self->{line_prev},
                    column => $self->{column_prev},
                   });

          redo A;
        } elsif ($self->{nc} == 0x003F) { # ?
          if ($self->{is_xml}) {
            
            $self->{state} = PI_STATE;
            
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
            redo A;
          } else {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'pio',
                            line => $self->{line_prev},
                            column => $self->{column_prev});
            $self->{state} = BOGUS_COMMENT_STATE;
            $self->{ct} = {type => COMMENT_TOKEN, data => '',
                           line => $self->{line_prev},
                           column => $self->{column_prev},
                          };
            ## $self->{nc} is intentionally left as is
            redo A;
          }
        } elsif (not $self->{is_xml} or $is_space->{$self->{nc}}) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare stago',
                          line => $self->{line_prev},
                          column => $self->{column_prev});
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## reconsume

          return  ({type => CHARACTER_TOKEN, data => '<',
                    line => $self->{line_prev},
                    column => $self->{column_prev},
                   });

          redo A;
        } else {
          ## XML5: "<:" is a parse error.
          
          $self->{ct} = {type => START_TAG_TOKEN,
                                    tag_name => chr ($self->{nc}),
                                    line => $self->{line_prev},
                                    column => $self->{column_prev}};
          $self->{state} = TAG_NAME_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        }
      } else {
        die "$0: $self->{content_model} in tag open";
      }
    } elsif ($self->{state} == CLOSE_TAG_OPEN_STATE) {
      ## NOTE: The "close tag open state" in the spec is implemented as
      ## |CLOSE_TAG_OPEN_STATE| and |CDATA_RCDATA_CLOSE_TAG_STATE|.

      ## XML5: "end tag state".

      my ($l, $c) = ($self->{line_prev}, $self->{column_prev} - 1); # "<"of"</"
      if ($self->{content_model} & CM_LIMITED_MARKUP) { # RCDATA | CDATA
        if (defined $self->{last_stag_name}) {
          $self->{state} = CDATA_RCDATA_CLOSE_TAG_STATE;
          $self->{kwd} = '';
          ## Reconsume.
          redo A;
        } else {
          ## No start tag token has ever been emitted
          ## NOTE: See <http://krijnhoetmer.nl/irc-logs/whatwg/20070626#l-564>.
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## Reconsume.
          return  ({type => CHARACTER_TOKEN, data => '</',
                    line => $l, column => $c,
                   });
          redo A;
        }
      }

      if (0x0041 <= $self->{nc} and
          $self->{nc} <= 0x005A) { # A..Z
        
        $self->{ct}
            = {type => END_TAG_TOKEN,
               tag_name => chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020)),
               line => $l, column => $c};
        $self->{state} = TAG_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif (0x0061 <= $self->{nc} and
               $self->{nc} <= 0x007A) { # a..z
        
        $self->{ct} = {type => END_TAG_TOKEN,
                                  tag_name => chr ($self->{nc}),
                                  line => $l, column => $c};
        $self->{state} = TAG_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'empty end tag',
                        line => $self->{line_prev}, ## "<" in "</>"
                        column => $self->{column_prev} - 1);
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        if ($self->{is_xml}) {
          
          ## XML5: No parse error.
          
          ## NOTE: This parser raises a parse error, since it supports
          ## XML1, not XML5.

          ## NOTE: A short end tag token.
          my $ct = {type => END_TAG_TOKEN,
                    tag_name => '',
                    line => $self->{line_prev},
                    column => $self->{column_prev} - 1,
                   };
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          return  ($ct);
        } else {
          
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        }
        redo A;
      } elsif ($self->{nc} == -1) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare etago');
        $self->{s_kwd} = '';
        $self->{state} = DATA_STATE;
        # reconsume

        return  ({type => CHARACTER_TOKEN, data => '</',
                  line => $l, column => $c,
                 });

        redo A;
      } elsif (not $self->{is_xml} or
               $is_space->{$self->{nc}}) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus end tag',
                        line => $self->{line_prev}, # "<" of "</"
                        column => $self->{column_prev} - 1);
        $self->{state} = BOGUS_COMMENT_STATE;
        $self->{ct} = {type => COMMENT_TOKEN, data => '',
                                  line => $self->{line_prev}, # "<" of "</"
                                  column => $self->{column_prev} - 1,
                                 };
        ## NOTE: $self->{nc} is intentionally left as is.
        ## Although the "anything else" case of the spec not explicitly
        ## states that the next input character is to be reconsumed,
        ## it will be included to the |data| of the comment token
        ## generated from the bogus end tag, as defined in the
        ## "bogus comment state" entry.
        redo A;
      } else {
        ## XML5: "</:" is a parse error.
        
        $self->{ct} = {type => END_TAG_TOKEN,
                       tag_name => chr ($self->{nc}),
                       line => $l, column => $c};
        $self->{state} = TAG_NAME_STATE; ## XML5: "end tag name state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == CDATA_RCDATA_CLOSE_TAG_STATE) {
      my $ch = substr $self->{last_stag_name}, length $self->{kwd}, 1;
      if (length $ch) {
        my $CH = $ch;
        $ch =~ tr/a-z/A-Z/;
        my $nch = chr $self->{nc};
        if ($nch eq $ch or $nch eq $CH) {
          
          ## Stay in the state.
          $self->{kwd} .= $nch;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## Reconsume.
          return  ({type => CHARACTER_TOKEN,
                    data => '</' . $self->{kwd},
                    line => $self->{line_prev},
                    column => $self->{column_prev} - 1 - length $self->{kwd},
                   });
          redo A;
        }
      } else { # after "<{tag-name}"
        unless ($is_space->{$self->{nc}} or
	        {
                 0x003E => 1, # >
                 0x002F => 1, # /
                 -1 => 1, # EOF
                }->{$self->{nc}}) {
          
          ## Reconsume.
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          return  ({type => CHARACTER_TOKEN,
                    data => '</' . $self->{kwd},
                    line => $self->{line_prev},
                    column => $self->{column_prev} - 1 - length $self->{kwd},
                   });
          redo A;
        } else {
          
          $self->{ct}
              = {type => END_TAG_TOKEN,
                 tag_name => $self->{last_stag_name},
                 line => $self->{line_prev},
                 column => $self->{column_prev} - 1 - length $self->{kwd}};
          $self->{state} = TAG_NAME_STATE;
          ## Reconsume.
          redo A;
        }
      }
    } elsif ($self->{state} == TAG_NAME_STATE) {
      if ($is_space->{$self->{nc}}) {
        
        $self->{state} = BEFORE_ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          #if ($self->{ct}->{attributes}) {
          #  ## NOTE: This should never be reached.
          #  !!! cp (36);
          #  !!! parse-error (type => 'end tag attribute');
          #} else {
            
          #}
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif (0x0041 <= $self->{nc} and
               $self->{nc} <= 0x005A) { # A..Z
        
        $self->{ct}->{tag_name}
            .= chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020));
          # start tag or end tag
        ## Stay in this state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          #if ($self->{ct}->{attributes}) {
          #  ## NOTE: This state should never be reached.
          #  !!! cp (40);
          #  !!! parse-error (type => 'end tag attribute');
          #} else {
            
          #}
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        # reconsume

        ## Discard the token.
        #return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif ($self->{nc} == 0x002F) { # /
        
        $self->{state} = SELF_CLOSING_START_TAG_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        
        $self->{ct}->{tag_name} .= chr $self->{nc};
          # start tag or end tag
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == BEFORE_ATTRIBUTE_NAME_STATE) {
      ## XML5: "Tag attribute name before state".

      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif (0x0041 <= $self->{nc} and
               $self->{nc} <= 0x005A) { # A..Z
        
        $self->{ca}
            = {name => chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020)),
               value => '',
               line => $self->{line}, column => $self->{column}};
        $self->{state} = ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x002F) { # /
        
        $self->{state} = SELF_CLOSING_START_TAG_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        # reconsume

        ## Discard the token.
        #return  ($self->{ct}); # start tag or end tag

        redo A;
      } else {
        if ({
             0x0022 => 1, # "
             0x0027 => 1, # '
             0x003C => 1, # <
             0x003D => 1, # =
            }->{$self->{nc}}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad attribute name');
        } else {
          
          ## XML5: ":" raises a parse error and is ignored.
        }
        $self->{ca}
            = {name => chr ($self->{nc}),
               value => '',
               line => $self->{line}, column => $self->{column}};
        $self->{state} = ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == ATTRIBUTE_NAME_STATE) {
      ## XML5: "Tag attribute name state".

      my $before_leave = sub {
        if (exists $self->{ct}->{attributes} # start tag or end tag
            ->{$self->{ca}->{name}}) { # MUST
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'duplicate attribute', text => $self->{ca}->{name}, line => $self->{ca}->{line}, column => $self->{ca}->{column});
          ## Discard $self->{ca} # MUST
        } else {
          
          $self->{ct}->{attributes}->{$self->{ca}->{name}}
            = $self->{ca};
          $self->{ca}->{index} = ++$self->{ct}->{last_index};
        }
      }; # $before_leave

      if ($is_space->{$self->{nc}}) {
        
        $before_leave->();
        $self->{state} = AFTER_ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003D) { # =
        
        $before_leave->();
        $self->{state} = BEFORE_ATTRIBUTE_VALUE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{is_xml}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr value'); ## TODO: type
        } else {
          
        }

        $before_leave->();
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif (0x0041 <= $self->{nc} and
               $self->{nc} <= 0x005A) { # A..Z
        
        $self->{ca}->{name}
            .= chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020));
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x002F) { # /
        if ($self->{is_xml}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr value'); ## TODO: type
        } else {
          
        }
        
        $before_leave->();
        $self->{state} = SELF_CLOSING_START_TAG_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
        $before_leave->();
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        # reconsume

        ## Discard the token.
        #return  ($self->{ct}); # start tag or end tag

        redo A;
      } else {
        if ({
             0x0022 => 1, # "
             0x0027 => 1, # '
             0x003C => 1, # <
            }->{$self->{nc}}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad attribute name');
        } else {
          
        }
        $self->{ca}->{name} .= chr ($self->{nc});
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_ATTRIBUTE_NAME_STATE) {
      ## XML5: "Tag attribute name after state".
      
      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003D) { # =
        
        $self->{state} = BEFORE_ATTRIBUTE_VALUE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{is_xml}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr value'); ## TODO: type
        } else {
          
        }

        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif (0x0041 <= $self->{nc} and
               $self->{nc} <= 0x005A) { # A..Z
        
        $self->{ca}
            = {name => chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020)),
               value => '',
               line => $self->{line}, column => $self->{column}};
        $self->{state} = ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x002F) { # /
        if ($self->{is_xml}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr value'); ## TODO: type
        } else {
          
        }
        
        $self->{state} = SELF_CLOSING_START_TAG_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{s_kwd} = '';
        $self->{state} = DATA_STATE;
        # reconsume

        ## Discard the token.
        #return  ($self->{ct}); # start tag or end tag

        redo A;
      } else {
        if ($self->{is_xml}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr value'); ## TODO: type
        } else {
          
        }

        if ({
             0x0022 => 1, # "
             0x0027 => 1, # '
             0x003C => 1, # <
            }->{$self->{nc}}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad attribute name');
        } else {
          
        }
        $self->{ca}
            = {name => chr ($self->{nc}),
               value => '',
               line => $self->{line}, column => $self->{column}};
        $self->{state} = ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;        
      }
    } elsif ($self->{state} == BEFORE_ATTRIBUTE_VALUE_STATE) {
      ## XML5: "Tag attribute value before state".

      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0026) { # &
        
        $self->{state} = ATTRIBUTE_VALUE_UNQUOTED_STATE;
        ## reconsume
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'empty unquoted attribute value');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## reconsume

        ## Discard the token.
        #return  ($self->{ct}); # start tag or end tag

        redo A;
      } else {
        if ($self->{nc} == 0x003D or $self->{nc} == 0x003C) { # =, <
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad attribute value');
        } elsif ($self->{is_xml}) {
          
          ## XML5: No parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unquoted attr value'); ## TODO
        } else {
          
        }
        $self->{ca}->{value} .= chr ($self->{nc});
        $self->{state} = ATTRIBUTE_VALUE_UNQUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE) {
      ## XML5: "Tag attribute value double quoted state" and "DOCTYPE
      ## ATTLIST attribute value double quoted state".
      
      if ($self->{nc} == 0x0022) { # "
        if ($self->{ct}->{type} == ATTLIST_TOKEN) {
          
          ## XML5: "DOCTYPE ATTLIST name after state".
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = AFTER_ATTLIST_ATTR_VALUE_QUOTED_STATE;
        } else {
          
          ## XML5: "Tag attribute name before state".
          $self->{state} = AFTER_ATTRIBUTE_VALUE_QUOTED_STATE;
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0026) { # &
        
        ## XML5: Not defined yet.

        ## NOTE: In the spec, the tokenizer is switched to the 
        ## "entity in attribute value state".  In this implementation, the
        ## tokenizer is switched to the |ENTITY_STATE|, which is an
        ## implementation of the "consume a character reference" algorithm.
        $self->{prev_state} = $self->{state};
        $self->{entity_add} = 0x0022; # "
        $self->{state} = ENTITY_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{is_xml} and 
               $is_space->{$self->{nc}}) {
        
        $self->{ca}->{value} .= ' ';
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed attribute value');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## reconsume
          return  ($self->{ct}); # start tag
          redo A;
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## reconsume

          ## Discard the token.
          #return  ($self->{ct}); # end tag

          redo A;
        } elsif ($self->{ct}->{type} == ATTLIST_TOKEN) {
          ## XML5: No parse error above; not defined yet.
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
          ## Reconsume.

          ## Discard the token.
          #return  ($self->{ct}); # ATTLIST

          redo A;
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
      } else {
        ## XML5 [ATTLIST]: Not defined yet.
        if ($self->{is_xml} and $self->{nc} == 0x003C) { # <
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lt in attr value'); ## TODO: type
        } else {
          
        }
        $self->{ca}->{value} .= chr ($self->{nc});
        $self->{read_until}->($self->{ca}->{value},
                              qq["&<\x09\x0C\x20],
                              length $self->{ca}->{value});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE) {
      ## XML5: "Tag attribute value single quoted state" and "DOCTYPE
      ## ATTLIST attribute value single quoted state".

      if ($self->{nc} == 0x0027) { # '
        if ($self->{ct}->{type} == ATTLIST_TOKEN) {
          
          ## XML5: "DOCTYPE ATTLIST name after state".
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = AFTER_ATTLIST_ATTR_VALUE_QUOTED_STATE;
        } else {
          
          ## XML5: "Before attribute name state" (sic).
          $self->{state} = AFTER_ATTRIBUTE_VALUE_QUOTED_STATE;
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0026) { # &
        
        ## XML5: Not defined yet.

        ## NOTE: In the spec, the tokenizer is switched to the 
        ## "entity in attribute value state".  In this implementation, the
        ## tokenizer is switched to the |ENTITY_STATE|, which is an
        ## implementation of the "consume a character reference" algorithm.
        $self->{entity_add} = 0x0027; # '
        $self->{prev_state} = $self->{state};
        $self->{state} = ENTITY_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{is_xml} and 
               $is_space->{$self->{nc}}) {
        
        $self->{ca}->{value} .= ' ';
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed attribute value');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## reconsume

          ## Discard the token.
          #return  ($self->{ct}); # start tag

          redo A;
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## reconsume

          ## Discard the token.
          #return  ($self->{ct}); # end tag

          redo A;
        } elsif ($self->{ct}->{type} == ATTLIST_TOKEN) {
          ## XML5: No parse error above; not defined yet.
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
          ## Reconsume.

          ## Discard the token.
          #return  ($self->{ct}); # ATTLIST

          redo A;
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
      } else {
        ## XML5 [ATTLIST]: Not defined yet.
        if ($self->{is_xml} and $self->{nc} == 0x003C) { # <
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lt in attr value'); ## TODO: type
        } else {
          
        }
        $self->{ca}->{value} .= chr ($self->{nc});
        $self->{read_until}->($self->{ca}->{value},
                              qq['&<\x09\x0C\x20],
                              length $self->{ca}->{value});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == ATTRIBUTE_VALUE_UNQUOTED_STATE) {
      ## XML5: "Tag attribute value unquoted state".

      if ($is_space->{$self->{nc}}) {
        if ($self->{ct}->{type} == ATTLIST_TOKEN) {
          
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = DOCTYPE_ATTLIST_NAME_AFTER_STATE;
        } else {
          
          ## XML5: "Tag attribute name before state".
          $self->{state} = BEFORE_ATTRIBUTE_NAME_STATE;
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0026) { # &
        

        ## XML5: Not defined yet.

        ## NOTE: In the spec, the tokenizer is switched to the 
        ## "entity in attribute value state".  In this implementation, the
        ## tokenizer is switched to the |ENTITY_STATE|, which is an
        ## implementation of the "consume a character reference" algorithm.
        $self->{entity_add} = -1;
        $self->{prev_state} = $self->{state};
        $self->{state} = ENTITY_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          return  ($self->{ct}); # start tag
          redo A;
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          return  ($self->{ct}); # end tag
          redo A;
        } elsif ($self->{ct}->{type} == ATTLIST_TOKEN) {
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          return  ($self->{ct}); # ATTLIST
          redo A;
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
      } elsif ($self->{nc} == -1) {
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
          $self->{last_stag_name} = $self->{ct}->{tag_name};

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## reconsume

          ## Discard the token.
          #return  ($self->{ct}); # start tag
          
          redo A;
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }

          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          ## reconsume

          ## Discard the token.
          #return  ($self->{ct}); # end tag

          redo A;
        } elsif ($self->{ct}->{type} == ATTLIST_TOKEN) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
          ## Reconsume.

          ## Discard the token.
          #return  ($self->{ct}); # ATTLIST

          redo A;
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
      } else {
        if ({
             0x0022 => 1, # "
             0x0027 => 1, # '
             0x003D => 1, # =
             0x003C => 1, # <
            }->{$self->{nc}}) {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad attribute value');
        } else {
          
        }
        $self->{ca}->{value} .= chr ($self->{nc});
        $self->{read_until}->($self->{ca}->{value},
                              qq["'=& \x09\x0C>],
                              length $self->{ca}->{value});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_ATTRIBUTE_VALUE_QUOTED_STATE) {
      if ($is_space->{$self->{nc}}) {
        
        $self->{state} = BEFORE_ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif ($self->{nc} == 0x002F) { # /
        
        $self->{state} = SELF_CLOSING_START_TAG_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag'); 
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## Reconsume.

        ## Discard the token.
        #return  ($self->{ct}); # start tag or end tag

        redo A;
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space between attributes');
        $self->{state} = BEFORE_ATTRIBUTE_NAME_STATE;
        ## reconsume
        redo A;
      }
    } elsif ($self->{state} == SELF_CLOSING_START_TAG_STATE) {
      ## XML5: "Empty tag state".

      if ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == END_TAG_TOKEN) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'nestc', token => $self->{ct});
          ## TODO: Different type than slash in start tag
          $self->{content_model} = PCDATA_CONTENT_MODEL; # MUST
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            
          }
          ## TODO: Test |<title></title/>|
        } else {
          
          $self->{self_closing} = 1;
        }

        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # start tag or end tag

        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed tag');
        if ($self->{ct}->{type} == START_TAG_TOKEN) {
          
          $self->{last_stag_name} = $self->{ct}->{tag_name};
        } elsif ($self->{ct}->{type} == END_TAG_TOKEN) {
          if ($self->{ct}->{attributes}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'end tag attribute');
          } else {
            ## NOTE: This state should never be reached.
            
          }
        } else {
          die "$0: $self->{ct}->{type}: Unknown token type";
        }
        ## XML5: "Tag attribute name before state".
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## Reconsume.

        ## Discard the token.
        #return  ($self->{ct}); # start tag or end tag

        redo A;
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'nestc');
        ## TODO: This error type is wrong.
        $self->{state} = BEFORE_ATTRIBUTE_NAME_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == BOGUS_COMMENT_STATE) {
      ## XML5: "Bogus comment state" and "DOCTYPE bogus comment state".

      ## NOTE: Unlike spec's "bogus comment state", this implementation
      ## consumes characters one-by-one basis.
      
      if ($self->{nc} == 0x003E) { # >
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # comment
        redo A;
      } elsif ($self->{nc} == -1) { 
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## reconsume

        return  ($self->{ct}); # comment
        redo A;
      } else {
        
        $self->{ct}->{data} .= chr ($self->{nc}); # comment
        $self->{read_until}->($self->{ct}->{data},
                              q[>],
                              length $self->{ct}->{data});

        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == MARKUP_DECLARATION_OPEN_STATE) {
      ## XML5: "Markup declaration state".
      
      if ($self->{nc} == 0x002D) { # -
        
        $self->{state} = MD_HYPHEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0044 or # D
               $self->{nc} == 0x0064) { # d
        ## ASCII case-insensitive.
        
        $self->{state} = MD_DOCTYPE_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((($self->{insertion_mode} & IN_FOREIGN_CONTENT_IM and
                 $self->{open_elements}->[-1]->[1] & FOREIGN_EL) or
                $self->{is_xml}) and
               $self->{nc} == 0x005B) { # [
                        
        $self->{state} = MD_CDATA_STATE;
        $self->{kwd} = '[';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        
      }

      $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                      line => $self->{line_prev},
                      column => $self->{column_prev} - 1);
      ## Reconsume.
      $self->{state} = BOGUS_COMMENT_STATE;
      $self->{ct} = {type => COMMENT_TOKEN, data => '',
                                line => $self->{line_prev},
                                column => $self->{column_prev} - 1,
                               };
      redo A;
    } elsif ($self->{state} == MD_HYPHEN_STATE) {
      if ($self->{nc} == 0x002D) { # -
        
        $self->{ct} = {type => COMMENT_TOKEN, data => '',
                                  line => $self->{line_prev},
                                  column => $self->{column_prev} - 2,
                                 };
        $self->{state} = COMMENT_START_STATE; ## XML5: "comment state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 2);
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN,
                                  data => '-',
                                  line => $self->{line_prev},
                                  column => $self->{column_prev} - 2,
                                 };
        redo A;
      }
    } elsif ($self->{state} == MD_DOCTYPE_STATE) {
      ## ASCII case-insensitive.
      if ($self->{nc} == [
            undef,
            0x004F, # O
            0x0043, # C
            0x0054, # T
            0x0059, # Y
            0x0050, # P
          ]->[length $self->{kwd}] or
          $self->{nc} == [
            undef,
            0x006F, # o
            0x0063, # c
            0x0074, # t
            0x0079, # y
            0x0070, # p
          ]->[length $self->{kwd}]) {
        
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 6 and
               ($self->{nc} == 0x0045 or # E
                $self->{nc} == 0x0065)) { # e
        if ($self->{is_xml} and
            ($self->{kwd} ne 'DOCTYP' or $self->{nc} == 0x0065)) {
          
          ## XML5: case-sensitive.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO
                          text => 'DOCTYPE',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 5);
        } else {
          
        }
        $self->{state} = DOCTYPE_STATE;
        $self->{ct} = {type => DOCTYPE_TOKEN,
                                  quirks => 1,
                                  line => $self->{line_prev},
                                  column => $self->{column_prev} - 7,
                                 };
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
                
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 1 - length $self->{kwd});
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN,
                                  data => $self->{kwd},
                                  line => $self->{line_prev},
                                  column => $self->{column_prev} - 1 - length $self->{kwd},
                                 };
        redo A;
      }
    } elsif ($self->{state} == MD_CDATA_STATE) {
      if ($self->{nc} == {
            '[' => 0x0043, # C
            '[C' => 0x0044, # D
            '[CD' => 0x0041, # A
            '[CDA' => 0x0054, # T
            '[CDAT' => 0x0041, # A
          }->{$self->{kwd}}) {
        
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{kwd} eq '[CDATA' and
               $self->{nc} == 0x005B) { # [
        if ($self->{is_xml} and 
            not $self->{tainted} and
            @{$self->{open_elements} or []} == 0) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'cdata outside of root element',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 7);
          $self->{tainted} = 1;
        } else {
          
        }

        $self->{ct} = {type => CHARACTER_TOKEN,
                                  data => '',
                                  line => $self->{line_prev},
                                  column => $self->{column_prev} - 7};
        $self->{state} = CDATA_SECTION_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 1 - length $self->{kwd});
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN,
                                  data => $self->{kwd},
                                  line => $self->{line_prev},
                                  column => $self->{column_prev} - 1 - length $self->{kwd},
                                 };
        redo A;
      }
    } elsif ($self->{state} == COMMENT_START_STATE) {
      if ($self->{nc} == 0x002D) { # -
        
        $self->{state} = COMMENT_START_DASH_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # comment

        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## reconsume

        return  ($self->{ct}); # comment

        redo A;
      } else {
        
        $self->{ct}->{data} # comment
            .= chr ($self->{nc});
        $self->{state} = COMMENT_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == COMMENT_START_DASH_STATE) {
      if ($self->{nc} == 0x002D) { # -
        
        $self->{state} = COMMENT_END_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # comment

        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## reconsume

        return  ($self->{ct}); # comment

        redo A;
      } else {
        
        $self->{ct}->{data} # comment
            .= '-' . chr ($self->{nc});
        $self->{state} = COMMENT_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == COMMENT_STATE) {
      ## XML5: "Comment state" and "DOCTYPE comment state".

      if ($self->{nc} == 0x002D) { # -
        
        $self->{state} = COMMENT_END_DASH_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## reconsume

        return  ($self->{ct}); # comment

        redo A;
      } else {
        
        $self->{ct}->{data} .= chr ($self->{nc}); # comment
        $self->{read_until}->($self->{ct}->{data},
                              q[-],
                              length $self->{ct}->{data});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == COMMENT_END_DASH_STATE) {
      ## XML5: "Comment dash state" and "DOCTYPE comment dash state".

      if ($self->{nc} == 0x002D) { # -
        
        $self->{state} = COMMENT_END_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## reconsume

        return  ($self->{ct}); # comment

        redo A;
      } else {
        
        $self->{ct}->{data} .= '-' . chr ($self->{nc}); # comment
        $self->{state} = COMMENT_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == COMMENT_END_STATE or
             $self->{state} == COMMENT_END_BANG_STATE) {
      ## XML5: "Comment end state" and "DOCTYPE comment end state".
      ## (No comment end bang state.)

      if ($self->{nc} == 0x003E) { # >
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # comment

        redo A;
      } elsif ($self->{nc} == 0x002D) { # -
        if ($self->{state} == COMMENT_END_BANG_STATE) {
          
          $self->{ct}->{data} .= '--!'; # comment
          $self->{state} = COMMENT_END_DASH_STATE;
        } else {
          
          ## XML5: Not a parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'dash in comment',
                          line => $self->{line_prev},
                          column => $self->{column_prev});
          $self->{ct}->{data} .= '-'; # comment
          ## Stay in the state
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{state} != COMMENT_END_BANG_STATE and
               $is_space->{$self->{nc}}) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'comment end space'); # XXX error type
        $self->{ct}->{data} .= '--' . chr ($self->{nc}); # comment
        $self->{state} = COMMENT_END_SPACE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{state} != COMMENT_END_BANG_STATE and
               $self->{nc} == 0x0021) { # !
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'comment end bang'); # XXX error type
        $self->{state} = COMMENT_END_BANG_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## Reconsume.

        return  ($self->{ct}); # comment

        redo A;
      } else {
        
        if ($self->{state} == COMMENT_END_BANG_STATE) {
          $self->{ct}->{data} .= '--!' . chr ($self->{nc}); # comment
        } else {
          $self->{ct}->{data} .= '--' . chr ($self->{nc}); # comment
        }
        $self->{state} = COMMENT_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } 
    } elsif ($self->{state} == COMMENT_END_SPACE_STATE) {
      ## XML5: Not exist.

      if ($self->{nc} == 0x003E) { # >
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # comment

        redo A;
      } elsif ($is_space->{$self->{nc}}) {
        
        $self->{ct}->{data} .= chr ($self->{nc}); # comment
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed comment');
        if ($self->{in_subset}) {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## Reconsume.

        return  ($self->{ct}); # comment

        redo A;
      } else {
        
        $self->{ct}->{data} .= chr ($self->{nc}); # comment
        $self->{state} = COMMENT_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_STATE) {
      if ($is_space->{$self->{nc}}) {
        
        $self->{state} = BEFORE_DOCTYPE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
        $self->{ct}->{quirks} = 1;

        $self->{state} = DATA_STATE;
        ## Reconsume.
        return  ($self->{ct}); # DOCTYPE (quirks)

        redo A;
      } else {
        
        ## XML5: Swith to the bogus comment state.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before DOCTYPE name');
        $self->{state} = BEFORE_DOCTYPE_NAME_STATE;
        ## reconsume
        redo A;
      }
    } elsif ($self->{state} == BEFORE_DOCTYPE_NAME_STATE) {
      ## XML5: "DOCTYPE root name before state".

      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no DOCTYPE name');
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # DOCTYPE (quirks)

        redo A;
      } elsif (0x0041 <= $self->{nc} and $self->{nc} <= 0x005A) { # A..Z
        
        $self->{ct}->{name} # DOCTYPE
            = chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020));
        delete $self->{ct}->{quirks};
        $self->{state} = DOCTYPE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no DOCTYPE name');
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## reconsume

        return  ($self->{ct}); # DOCTYPE (quirks)

        redo A;
      } elsif ($self->{is_xml} and $self->{nc} == 0x005B) { # [
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no DOCTYPE name');
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        
        $self->{ct}->{name} = chr $self->{nc};
        delete $self->{ct}->{quirks};
        $self->{state} = DOCTYPE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_NAME_STATE) {
      ## XML5: "DOCTYPE root name state".

      ## ISSUE: Redundant "First," in the spec.

      if ($is_space->{$self->{nc}}) {
        
        $self->{state} = AFTER_DOCTYPE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # DOCTYPE

        redo A;
      } elsif (0x0041 <= $self->{nc} and $self->{nc} <= 0x005A) { # A..Z
        
        $self->{ct}->{name} # DOCTYPE
            .= chr ($self->{nc} + ($self->{is_xml} ? 0 : 0x0020));
        delete $self->{ct}->{quirks};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## reconsume

        $self->{ct}->{quirks} = 1;
        return  ($self->{ct}); # DOCTYPE

        redo A;
      } elsif ($self->{is_xml} and $self->{nc} == 0x005B) { # [
        
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        
        $self->{ct}->{name} .= chr ($self->{nc}); # DOCTYPE
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_DOCTYPE_NAME_STATE) {
      ## XML5: Corresponding to XML5's "DOCTYPE root name after
      ## state", but implemented differently.

      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no md def'); ## TODO: type
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        ## Reconsume.
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == 0x0050 or # P
               $self->{nc} == 0x0070) { # p
        
        $self->{state} = PUBLIC_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0053 or # S
               $self->{nc} == 0x0073) { # s
        
        $self->{state} = SYSTEM_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022 and # "
               ($self->{ct}->{type} == GENERAL_ENTITY_TOKEN or
                $self->{ct}->{type} == PARAMETER_ENTITY_TOKEN)) {
        
        $self->{state} = DOCTYPE_ENTITY_VALUE_DOUBLE_QUOTED_STATE;
        $self->{ct}->{value} = ''; # ENTITY
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027 and # '
               ($self->{ct}->{type} == GENERAL_ENTITY_TOKEN or
                $self->{ct}->{type} == PARAMETER_ENTITY_TOKEN)) {
        
        $self->{state} = DOCTYPE_ENTITY_VALUE_SINGLE_QUOTED_STATE;
        $self->{ct}->{value} = ''; # ENTITY
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{is_xml} and
               $self->{ct}->{type} == DOCTYPE_TOKEN and
               $self->{nc} == 0x005B) { # [
        
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after DOCTYPE name'); ## TODO: type

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{ct}->{quirks} = 1;
          $self->{state} = BOGUS_DOCTYPE_STATE;
        } else {
          
          $self->{state} = BOGUS_MD_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == PUBLIC_STATE) {
      ## ASCII case-insensitive
      if ($self->{nc} == [
            undef, 
            0x0055, # U
            0x0042, # B
            0x004C, # L
            0x0049, # I
          ]->[length $self->{kwd}] or
          $self->{nc} == [
            undef, 
            0x0075, # u
            0x0062, # b
            0x006C, # l
            0x0069, # i
          ]->[length $self->{kwd}]) {
        
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 5 and
               ($self->{nc} == 0x0043 or # C
                $self->{nc} == 0x0063)) { # c
        if ($self->{is_xml} and
            ($self->{kwd} ne 'PUBLI' or $self->{nc} == 0x0063)) { # c
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO: type
                          text => 'PUBLIC',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 4);
        } else {
          
        }
        $self->{state} = BEFORE_DOCTYPE_PUBLIC_IDENTIFIER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after DOCTYPE name', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev} + 1 - length $self->{kwd});
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{ct}->{quirks} = 1;
          $self->{state} = BOGUS_DOCTYPE_STATE;
        } else {
          
          $self->{state} = BOGUS_MD_STATE;
        }
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == SYSTEM_STATE) {
      ## ASCII case-insensitive
      if ($self->{nc} == [
            undef, 
            0x0059, # Y
            0x0053, # S
            0x0054, # T
            0x0045, # E
          ]->[length $self->{kwd}] or
          $self->{nc} == [
            undef, 
            0x0079, # y
            0x0073, # s
            0x0074, # t
            0x0065, # e
          ]->[length $self->{kwd}]) {
        
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 5 and
               ($self->{nc} == 0x004D or # M
                $self->{nc} == 0x006D)) { # m
        if ($self->{is_xml} and
            ($self->{kwd} ne 'SYSTE' or $self->{nc} == 0x006D)) { # m
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO: type
                          text => 'SYSTEM',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 4);
        } else {
          
        }
        $self->{state} = BEFORE_DOCTYPE_SYSTEM_IDENTIFIER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after DOCTYPE name', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev} + 1 - length $self->{kwd});
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{ct}->{quirks} = 1;
          $self->{state} = BOGUS_DOCTYPE_STATE;
        } else {
          
          $self->{state} = BOGUS_MD_STATE;
        }
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == BEFORE_DOCTYPE_PUBLIC_IDENTIFIER_STATE) {
      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} eq 0x0022) { # "
        
        $self->{ct}->{pubid} = ''; # DOCTYPE
        $self->{state} = DOCTYPE_PUBLIC_IDENTIFIER_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} eq 0x0027) { # '
        
        $self->{ct}->{pubid} = ''; # DOCTYPE
        $self->{state} = DOCTYPE_PUBLIC_IDENTIFIER_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} eq 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no PUBLIC literal');
        
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        ## reconsume
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } elsif ($self->{is_xml} and
               $self->{ct}->{type} == DOCTYPE_TOKEN and
               $self->{nc} == 0x005B) { # [
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no PUBLIC literal');
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after PUBLIC');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{ct}->{quirks} = 1;
          $self->{state} = BOGUS_DOCTYPE_STATE;
        } else {
          
          $self->{state} = BOGUS_MD_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_PUBLIC_IDENTIFIER_DOUBLE_QUOTED_STATE) {
      if ($self->{nc} == 0x0022) { # "
        
        $self->{state} = AFTER_DOCTYPE_PUBLIC_IDENTIFIER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed PUBLIC literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed PUBLIC literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        ## Reconsume.
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        
        $self->{ct}->{pubid} .= chr $self->{nc}; # DOCTYPE/ENTITY/NOTATION
        $self->{read_until}->($self->{ct}->{pubid}, q[">],
                              length $self->{ct}->{pubid});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_PUBLIC_IDENTIFIER_SINGLE_QUOTED_STATE) {
      if ($self->{nc} == 0x0027) { # '
        
        $self->{state} = AFTER_DOCTYPE_PUBLIC_IDENTIFIER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed PUBLIC literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed PUBLIC literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
      
        ## reconsume
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } else {
        
        $self->{ct}->{pubid} .= chr $self->{nc}; # DOCTYPE/ENTITY/NOTATION
        $self->{read_until}->($self->{ct}->{pubid}, q['>],
                              length $self->{ct}->{pubid});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_DOCTYPE_PUBLIC_IDENTIFIER_STATE) {
      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        
        $self->{ct}->{sysid} = ''; # DOCTYPE/ENTITY/NOTATION
        $self->{state} = DOCTYPE_SYSTEM_IDENTIFIER_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        
        $self->{ct}->{sysid} = ''; # DOCTYPE/ENTITY/NOTATION
        $self->{state} = DOCTYPE_SYSTEM_IDENTIFIER_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          if ($self->{is_xml}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'no SYSTEM literal');
          } else {
            
          }
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        } else {
          if ($self->{ct}->{type} == NOTATION_TOKEN) {
            
          } else {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'no SYSTEM literal');            
          }
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        ## reconsume
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{is_xml} and
               $self->{ct}->{type} == DOCTYPE_TOKEN and
               $self->{nc} == 0x005B) { # [
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no SYSTEM literal');
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after PUBLIC literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{ct}->{quirks} = 1;
          $self->{state} = BOGUS_DOCTYPE_STATE;
        } else {
          
          $self->{state} = BOGUS_MD_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == BEFORE_DOCTYPE_SYSTEM_IDENTIFIER_STATE) {
      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        
        $self->{ct}->{sysid} = ''; # DOCTYPE
        $self->{state} = DOCTYPE_SYSTEM_IDENTIFIER_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        
        $self->{ct}->{sysid} = ''; # DOCTYPE
        $self->{state} = DOCTYPE_SYSTEM_IDENTIFIER_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no SYSTEM literal');
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }

        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        ## reconsume
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{is_xml} and
               $self->{ct}->{type} == DOCTYPE_TOKEN and
               $self->{nc} == 0x005B) { # [
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no SYSTEM literal');

        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after SYSTEM');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
                    
          $self->{ct}->{quirks} = 1;
          $self->{state} = BOGUS_DOCTYPE_STATE;
        } else {
          
          $self->{state} = BOGUS_MD_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_SYSTEM_IDENTIFIER_DOUBLE_QUOTED_STATE) {
      if ($self->{nc} == 0x0022) { # "
        
        $self->{state} = AFTER_DOCTYPE_SYSTEM_IDENTIFIER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif (not $self->{is_xml} and $self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed SYSTEM literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed SYSTEM literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }
        
        ## reconsume
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } else {
        
        $self->{ct}->{sysid} .= chr $self->{nc}; # DOCTYPE/ENTITY/NOTATION
        $self->{read_until}->($self->{ct}->{sysid}, q[">],
                              length $self->{ct}->{sysid});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_SYSTEM_IDENTIFIER_SINGLE_QUOTED_STATE) {
      if ($self->{nc} == 0x0027) { # '
        
        $self->{state} = AFTER_DOCTYPE_SYSTEM_IDENTIFIER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif (not $self->{is_xml} and $self->{nc} == 0x003E) { # >
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed SYSTEM literal');

        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        $self->{ct}->{quirks} = 1;
        return  ($self->{ct}); # DOCTYPE

        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed SYSTEM literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }

        ## reconsume
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } else {
        
        $self->{ct}->{sysid} .= chr $self->{nc}; # DOCTYPE/ENTITY/NOTATION
        $self->{read_until}->($self->{ct}->{sysid}, q['>],
                              length $self->{ct}->{sysid});

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_DOCTYPE_SYSTEM_IDENTIFIER_STATE) {
      if ($is_space->{$self->{nc}}) {
        if ($self->{ct}->{type} == GENERAL_ENTITY_TOKEN) {
          
          $self->{state} = BEFORE_NDATA_STATE;
        } else {
          
          ## Stay in the state
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        } else {
          
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{ct}->{type} == GENERAL_ENTITY_TOKEN and
               ($self->{nc} == 0x004E or # N
                $self->{nc} == 0x006E)) { # n
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before NDATA'); ## TODO: type
        $self->{state} = NDATA_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
          $self->{ct}->{quirks} = 1;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        }

        ## reconsume
        return  ($self->{ct}); # DOCTYPE/ENTITY/NOTATION
        redo A;
      } elsif ($self->{is_xml} and
               $self->{ct}->{type} == DOCTYPE_TOKEN and
               $self->{nc} == 0x005B) { # [
        
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after SYSTEM literal');

        if ($self->{ct}->{type} == DOCTYPE_TOKEN) {
          
          #$self->{ct}->{quirks} = 1;
          $self->{state} = BOGUS_DOCTYPE_STATE;
        } else {
          
          $self->{state} = BOGUS_MD_STATE;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == BEFORE_NDATA_STATE) {
      if ($is_space->{$self->{nc}}) {
        
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY
        redo A;
      } elsif ($self->{nc} == 0x004E or # N
               $self->{nc} == 0x006E) { # n
        
        $self->{state} = NDATA_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        ## reconsume
        return  ($self->{ct}); # ENTITY
        redo A;
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after SYSTEM literal');
        $self->{state} = BOGUS_MD_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == BOGUS_DOCTYPE_STATE) {
      if ($self->{nc} == 0x003E) { # >
        
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  

        return  ($self->{ct}); # DOCTYPE

        redo A;
      } elsif ($self->{is_xml} and $self->{nc} == 0x005B) { # [
        
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        $self->{ct}->{has_internal_subset} = 1; # DOCTYPE
        $self->{in_subset} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # DOCTYPE
        redo A;
      } elsif ($self->{nc} == -1) {
        
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## reconsume

        return  ($self->{ct}); # DOCTYPE

        redo A;
      } else {
        
        my $s = '';
        $self->{read_until}->($s, q{>[}, 0);

        ## Stay in the state
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == CDATA_SECTION_STATE) {
      ## NOTE: "CDATA section state" in the state is jointly implemented
      ## by three states, |CDATA_SECTION_STATE|, |CDATA_SECTION_MSE1_STATE|,
      ## and |CDATA_SECTION_MSE2_STATE|.

      ## XML5: "CDATA state".
      
      if ($self->{nc} == 0x005D) { # ]
        
        $self->{state} = CDATA_SECTION_MSE1_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        if ($self->{is_xml}) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no mse'); ## TODO: type
        } else {
          
        }

        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## Reconsume.
        if (length $self->{ct}->{data}) { # character
          
          return  ($self->{ct}); # character
        } else {
          
          ## No token to emit. $self->{ct} is discarded.
        }        
        redo A;
      } else {
        
        $self->{ct}->{data} .= chr $self->{nc};
        $self->{read_until}->($self->{ct}->{data},
                              q<]>,
                              length $self->{ct}->{data});

        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }

      ## ISSUE: "text tokens" in spec.
    } elsif ($self->{state} == CDATA_SECTION_MSE1_STATE) {
      ## XML5: "CDATA bracket state".

      if ($self->{nc} == 0x005D) { # ]
        
        $self->{state} = CDATA_SECTION_MSE2_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        
        ## XML5: If EOF, "]" is not appended and changed to the data state.
        $self->{ct}->{data} .= ']';
        $self->{state} = CDATA_SECTION_STATE; ## XML5: Stay in the state.
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == CDATA_SECTION_MSE2_STATE) {
      ## XML5: "CDATA end state".

      if ($self->{nc} == 0x003E) { # >
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        if (length $self->{ct}->{data}) { # character
          
          return  ($self->{ct}); # character
        } else {
          
          ## No token to emit. $self->{ct} is discarded.
        }
        redo A;
      } elsif ($self->{nc} == 0x005D) { # ]
         # character
        $self->{ct}->{data} .= ']'; ## Add first "]" of "]]]".
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        
        $self->{ct}->{data} .= ']]'; # character
        $self->{state} = CDATA_SECTION_STATE;
        ## Reconsume. ## XML5: Emit.
        redo A;
      }
    } elsif ($self->{state} == ENTITY_STATE) {
      if ($is_space->{$self->{nc}} or
          {
            0x003C => 1, 0x0026 => 1, -1 => 1, # <, &
            $self->{entity_add} => 1,
          }->{$self->{nc}}) {
        if ($self->{is_xml}) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare ero',
                          line => $self->{line_prev},
                          column => $self->{column_prev}
                              + ($self->{nc} == -1 ? 1 : 0));
        } else {
          
          ## No error
        }
        ## Don't consume
        ## Return nothing.
        #
      } elsif ($self->{nc} == 0x0023) { # #
        
        $self->{state} = ENTITY_HASH_STATE;
        $self->{kwd} = '#';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{is_xml} or
               (0x0041 <= $self->{nc} and
                $self->{nc} <= 0x005A) or # A..Z
               (0x0061 <= $self->{nc} and
                $self->{nc} <= 0x007A)) { # a..z
        
        require HTML::HTML5::Parser::NamedEntityList;
        $self->{state} = ENTITY_NAME_STATE;
        $self->{kwd} = chr $self->{nc};
        $self->{entity__value} = $self->{kwd};
        $self->{entity__match} = 0;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare ero');
        ## Return nothing.
        #
      }

      ## NOTE: No character is consumed by the "consume a character
      ## reference" algorithm.  In other word, there is an "&" character
      ## that does not introduce a character reference, which would be
      ## appended to the parent element or the attribute value in later
      ## process of the tokenizer.

      if ($self->{prev_state} == DATA_STATE) {
        
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        return  ({type => CHARACTER_TOKEN, data => '&',
                  line => $self->{line_prev},
                  column => $self->{column_prev},
                 });
        redo A;
      } else {
        
        $self->{ca}->{value} .= '&';
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == ENTITY_HASH_STATE) {
      if ($self->{nc} == 0x0078) { # x
        
        $self->{state} = HEXREF_X_STATE;
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0058) { # X
        
        if ($self->{is_xml}) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'uppercase hcro'); ## TODO: type
        }
        $self->{state} = HEXREF_X_STATE;
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif (0x0030 <= $self->{nc} and
               $self->{nc} <= 0x0039) { # 0..9
        
        $self->{state} = NCR_NUM_STATE;
        $self->{kwd} = $self->{nc} - 0x0030;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare nero',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 1);

        ## NOTE: According to the spec algorithm, nothing is returned,
        ## and then "&#" is appended to the parent element or the attribute 
        ## value in the later processing.

        if ($self->{prev_state} == DATA_STATE) {
          
          $self->{state} = $self->{prev_state};
          $self->{s_kwd} = '';
          ## Reconsume.
          return  ({type => CHARACTER_TOKEN,
                    data => '&#',
                    line => $self->{line_prev},
                    column => $self->{column_prev} - 1,
                   });
          redo A;
        } else {
          
          $self->{ca}->{value} .= '&#';
          $self->{state} = $self->{prev_state};
          $self->{s_kwd} = '';
          ## Reconsume.
          redo A;
        }
      }
    } elsif ($self->{state} == NCR_NUM_STATE) {
      if (0x0030 <= $self->{nc} and 
          $self->{nc} <= 0x0039) { # 0..9
        
        $self->{kwd} *= 10;
        $self->{kwd} += $self->{nc} - 0x0030;
        
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003B) { # ;
        
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        #
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no refc');
        ## Reconsume.
        #
      }

      my $code = $self->{kwd};
      my $l = $self->{line_prev};
      my $c = $self->{column_prev};
      if ((not $self->{is_xml} and $charref_map->{$code}) or
          ($self->{is_xml} and 0xD800 <= $code and $code <= 0xDFFF) or
          ($self->{is_xml} and $code == 0x0000)) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'invalid character reference',
                        text => (sprintf 'U+%04X', $code),
                        line => $l, column => $c);
        $code = $charref_map->{$code};
      } elsif ($code > 0x10FFFF) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'invalid character reference',
                        text => (sprintf 'U-%08X', $code),
                        line => $l, column => $c);
        $code = 0xFFFD;
      }

      if ($self->{prev_state} == DATA_STATE) {
        
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        return  ({type => CHARACTER_TOKEN, data => chr $code,
                  has_reference => 1,
                  line => $l, column => $c,
                 });
        redo A;
      } else {
        
        $self->{ca}->{value} .= chr $code;
        $self->{ca}->{has_reference} = 1;
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == HEXREF_X_STATE) {
      if ((0x0030 <= $self->{nc} and $self->{nc} <= 0x0039) or
          (0x0041 <= $self->{nc} and $self->{nc} <= 0x0046) or
          (0x0061 <= $self->{nc} and $self->{nc} <= 0x0066)) {
        # 0..9, A..F, a..f
        
        $self->{state} = HEXREF_HEX_STATE;
        $self->{kwd} = 0;
        ## Reconsume.
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare hcro',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 2);

        ## NOTE: According to the spec algorithm, nothing is returned,
        ## and then "&#" followed by "X" or "x" is appended to the parent
        ## element or the attribute value in the later processing.

        if ($self->{prev_state} == DATA_STATE) {
          
          $self->{state} = $self->{prev_state};
          $self->{s_kwd} = '';
          ## Reconsume.
          return  ({type => CHARACTER_TOKEN,
                    data => '&' . $self->{kwd},
                    line => $self->{line_prev},
                    column => $self->{column_prev} - length $self->{kwd},
                   });
          redo A;
        } else {
          
          $self->{ca}->{value} .= '&' . $self->{kwd};
          $self->{state} = $self->{prev_state};
          $self->{s_kwd} = '';
          ## Reconsume.
          redo A;
        }
      }
    } elsif ($self->{state} == HEXREF_HEX_STATE) {
      if (0x0030 <= $self->{nc} and $self->{nc} <= 0x0039) {
        # 0..9
        
        $self->{kwd} *= 0x10;
        $self->{kwd} += $self->{nc} - 0x0030;
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif (0x0061 <= $self->{nc} and
               $self->{nc} <= 0x0066) { # a..f
        
        $self->{kwd} *= 0x10;
        $self->{kwd} += $self->{nc} - 0x0060 + 9;
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif (0x0041 <= $self->{nc} and
               $self->{nc} <= 0x0046) { # A..F
        
        $self->{kwd} *= 0x10;
        $self->{kwd} += $self->{nc} - 0x0040 + 9;
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003B) { # ;
        
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        #
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no refc',
                        line => $self->{line},
                        column => $self->{column});
        ## Reconsume.
        #
      }

      my $code = $self->{kwd};
      my $l = $self->{line_prev};
      my $c = $self->{column_prev};
      if ((not $self->{is_xml} and $charref_map->{$code}) or
          ($self->{is_xml} and 0xD800 <= $code and $code <= 0xDFFF) or
          ($self->{is_xml} and $code == 0x0000)) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'invalid character reference',
                        text => (sprintf 'U+%04X', $code),
                        line => $l, column => $c);
        $code = $charref_map->{$code};
      } elsif ($code > 0x10FFFF) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'invalid character reference',
                        text => (sprintf 'U-%08X', $code),
                        line => $l, column => $c);
        $code = 0xFFFD;
      }

      if ($self->{prev_state} == DATA_STATE) {
        
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        return  ({type => CHARACTER_TOKEN, data => chr $code,
                  has_reference => 1,
                  line => $l, column => $c,
                 });
        redo A;
      } else {
        
        $self->{ca}->{value} .= chr $code;
        $self->{ca}->{has_reference} = 1;
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == ENTITY_NAME_STATE) {
      if ((0x0041 <= $self->{nc} and # a
           $self->{nc} <= 0x005A) or # x
          (0x0061 <= $self->{nc} and # a
           $self->{nc} <= 0x007A) or # z
          (0x0030 <= $self->{nc} and # 0
           $self->{nc} <= 0x0039) or # 9
          $self->{nc} == 0x003B or # ;
          ($self->{is_xml} and
           not ($is_space->{$self->{nc}} or
                {
                  0x003C => 1, 0x0026 => 1, -1 => 1, # <, &
                  $self->{entity_add} => 1,
                }->{$self->{nc}}))) {
        our $EntityChar;
        $self->{kwd} .= chr $self->{nc};
        if (defined $EntityChar->{$self->{kwd}} or
            $self->{ge}->{$self->{kwd}}) {
          if ($self->{nc} == 0x003B) { # ;
            if (defined $self->{ge}->{$self->{kwd}}) {
              if ($self->{ge}->{$self->{kwd}}->{only_text}) {
                
                $self->{entity__value} = $self->{ge}->{$self->{kwd}}->{value};
              } else {
                if (defined $self->{ge}->{$self->{kwd}}->{notation}) {
                  
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'unparsed entity', ## TODO: type
                                  value => $self->{kwd});
                } else {
                  
                }
                $self->{entity__value} = '&' . $self->{kwd}; ## TODO: expand
              }
            } else {
              if ($self->{is_xml}) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'entity not declared', ## TODO: type
                                value => $self->{kwd},
                                level => {
                                          'amp;' => $self->{level}->{warn},
                                          'quot;' => $self->{level}->{warn},
                                          'lt;' => $self->{level}->{warn},
                                          'gt;' => $self->{level}->{warn},
                                          'apos;' => $self->{level}->{warn},
                                         }->{$self->{kwd}} ||
                                         $self->{level}->{must});
              } else {
                
              }
              $self->{entity__value} = $EntityChar->{$self->{kwd}};
            }
            $self->{entity__match} = 1;
            
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
            #
          } else {
            
            $self->{entity__value} = $EntityChar->{$self->{kwd}};
            $self->{entity__match} = -1;
            ## Stay in the state.
            
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
            redo A;
          }
        } else {
          
          $self->{entity__value} .= chr $self->{nc};
          $self->{entity__match} *= 2;
          ## Stay in the state.
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        }
      }

      my $data;
      my $has_ref;
      if ($self->{entity__match} > 0) {
        
        $data = $self->{entity__value};
        $has_ref = 1;
        #
      } elsif ($self->{entity__match} < 0) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no refc');
        if ($self->{prev_state} != DATA_STATE and # in attribute
            $self->{entity__match} < -1) {
          
          $data = '&' . $self->{kwd};
          #
        } else {
          
          $data = $self->{entity__value};
          $has_ref = 1;
          #
        }
      } else {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare ero',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - length $self->{kwd});
        $data = '&' . $self->{kwd};
        #
      }
  
      ## NOTE: In these cases, when a character reference is found,
      ## it is consumed and a character token is returned, or, otherwise,
      ## nothing is consumed and returned, according to the spec algorithm.
      ## In this implementation, anything that has been examined by the
      ## tokenizer is appended to the parent element or the attribute value
      ## as string, either literal string when no character reference or
      ## entity-replaced string otherwise, in this stage, since any characters
      ## that would not be consumed are appended in the data state or in an
      ## appropriate attribute value state anyway.
 
      if ($self->{prev_state} == DATA_STATE) {
        
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        return  ({type => CHARACTER_TOKEN,
                  data => $data,
                  has_reference => $has_ref,
                  line => $self->{line_prev},
                  column => $self->{column_prev} + 1 - length $self->{kwd},
                 });
        redo A;
      } else {
        
        $self->{ca}->{value} .= $data;
        $self->{ca}->{has_reference} = 1 if $has_ref;
        $self->{state} = $self->{prev_state};
        $self->{s_kwd} = '';
        ## Reconsume.
        redo A;
      }

    ## XML-only states

    } elsif ($self->{state} == PI_STATE) {
      ## XML5: "Pi state" and "DOCTYPE pi state".

      if ($is_space->{$self->{nc}} or
          $self->{nc} == 0x003F or # ?
          $self->{nc} == -1) {
        ## XML5: U+003F: "pi state": Same as "Anything else"; "DOCTYPE
        ## pi state": Switch to the "DOCTYPE pi after state".  EOF:
        ## "DOCTYPE pi state": Parse error, switch to the "data
        ## state".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare pio', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev}
                            - 1 * ($self->{nc} != -1));
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN,
                       data => '?',
                       line => $self->{line_prev},
                       column => $self->{column_prev}
                           - 1 * ($self->{nc} != -1),
                      };
        redo A;
      } else {
        ## XML5: "DOCTYPE pi state": Stay in the state.
        $self->{ct} = {type => PI_TOKEN,
                       target => chr $self->{nc},
                       data => '',
                       line => $self->{line_prev},
                       column => $self->{column_prev} - 1,
                      };
        $self->{state} = PI_TARGET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == PI_TARGET_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = PI_TARGET_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no pic'); ## TODO: type
        if ($self->{in_subset}) {
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## Reconsume.
        return  ($self->{ct}); # pi
        redo A;
      } elsif ($self->{nc} == 0x003F) { # ?
        $self->{state} = PI_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        ## XML5: typo ("tag name" -> "target")
        $self->{ct}->{target} .= chr $self->{nc}; # pi
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == PI_TARGET_AFTER_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{state} = PI_DATA_STATE;
        ## Reprocess.
        redo A;
      }
    } elsif ($self->{state} == PI_DATA_STATE) {
      if ($self->{nc} == 0x003F) { # ?
        $self->{state} = PI_DATA_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no pic'); ## TODO: type
        if ($self->{in_subset}) {
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state"
        } else {
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        ## Reprocess.
        return  ($self->{ct}); # pi
        redo A;
      } else {
        $self->{ct}->{data} .= chr $self->{nc}; # pi
        $self->{read_until}->($self->{ct}->{data}, q[?],
                              length $self->{ct}->{data});
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        ## Reprocess.
        redo A;
      }
    } elsif ($self->{state} == PI_AFTER_STATE) {
      ## XML5: Part of "Pi after state".

      if ($self->{nc} == 0x003E) { # >
        if ($self->{in_subset}) {
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # pi
        redo A;
      } elsif ($self->{nc} == 0x003F) { # ?
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no s after target', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev}); ## XML5: no error
        $self->{ct}->{data} .= '?';
        $self->{state} = PI_DATA_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no s after target', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev}
                            + 1 * ($self->{nc} == -1)); ## XML5: no error
        $self->{ct}->{data} .= '?'; ## XML5: not appended
        $self->{state} = PI_DATA_STATE;
        ## Reprocess.
        redo A;
      }
    } elsif ($self->{state} == PI_DATA_AFTER_STATE) {
      ## XML5: Same as "pi after state" and "DOCTYPE pi after state".

      if ($self->{nc} == 0x003E) { # >
        if ($self->{in_subset}) {
          $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        } else {
          $self->{state} = DATA_STATE;
          $self->{s_kwd} = '';
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # pi
        redo A;
      } elsif ($self->{nc} == 0x003F) { # ?
        $self->{ct}->{data} .= '?';
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{ct}->{data} .= '?'; ## XML5: not appended
        $self->{state} = PI_DATA_STATE;
        ## Reprocess.
        redo A;
      }

    } elsif ($self->{state} == DOCTYPE_INTERNAL_SUBSET_STATE) {
      if ($self->{nc} == 0x003C) { # <
        $self->{state} = DOCTYPE_TAG_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0025) { # %
        ## XML5: Not defined yet.

        ## TODO:

        if (not $self->{stop_processing} and
            not $self->{document}->xml_standalone) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'stop processing', ## TODO: type
                          level => $self->{level}->{info});
          $self->{stop_processing} = 1;
        }

        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x005D) { # ]
        delete $self->{in_subset};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed internal subset'); ## TODO: type
        delete $self->{in_subset};
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## Reconsume.
        return  ({type => END_OF_DOCTYPE_TOKEN});
        redo A;
      } else {
        unless ($self->{internal_subset_tainted}) {
          ## XML5: No parse error.
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'string in internal subset');
          $self->{internal_subset_tainted} = 1;
        }
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_INTERNAL_SUBSET_AFTER_STATE) {
      if ($self->{nc} == 0x003E) { # >
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ({type => END_OF_DOCTYPE_TOKEN});
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed DOCTYPE');
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## Reconsume.
        return  ({type => END_OF_DOCTYPE_TOKEN});
        redo A;
      } else {
        ## XML5: No parse error and stay in the state.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after internal subset'); ## TODO: type

        $self->{state} = BOGUS_DOCTYPE_INTERNAL_SUBSET_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == BOGUS_DOCTYPE_INTERNAL_SUBSET_AFTER_STATE) {
      if ($self->{nc} == 0x003E) { # >
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ({type => END_OF_DOCTYPE_TOKEN});
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## Reconsume.
        return  ({type => END_OF_DOCTYPE_TOKEN});
        redo A;
      } else {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_TAG_STATE) {
      if ($self->{nc} == 0x0021) { # !
        $self->{state} = DOCTYPE_MARKUP_DECLARATION_OPEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003F) { # ?
        $self->{state} = PI_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare stago');
        $self->{state} = DATA_STATE;
        $self->{s_kwd} = '';
        ## Reconsume.
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare stago', ## XML5: Not a parse error.
                        line => $self->{line_prev},
                        column => $self->{column_prev});
        $self->{state} = BOGUS_COMMENT_STATE;
        $self->{ct} = {type => COMMENT_TOKEN,
                       data => '',
                      }; ## NOTE: Will be discarded.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_MARKUP_DECLARATION_OPEN_STATE) {
      ## XML5: "DOCTYPE markup declaration state".
      
      if ($self->{nc} == 0x002D) { # -
        $self->{state} = MD_HYPHEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0045 or # E
               $self->{nc} == 0x0065) { # e
        $self->{state} = MD_E_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0041 or # A
               $self->{nc} == 0x0061) { # a
        $self->{state} = MD_ATTLIST_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x004E or # N
               $self->{nc} == 0x006E) { # n
        $self->{state} = MD_NOTATION_STATE;
        $self->{kwd} = chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        #
      }
      
      ## XML5: No parse error.
      $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                      line => $self->{line_prev},
                      column => $self->{column_prev} - 1);
      ## Reconsume.
      $self->{state} = BOGUS_COMMENT_STATE;
      $self->{ct} = {type => COMMENT_TOKEN, data => ''}; ## Will be discarded.
      redo A;
    } elsif ($self->{state} == MD_E_STATE) {
      if ($self->{nc} == 0x004E or # N
          $self->{nc} == 0x006E) { # n
        $self->{state} = MD_ENTITY_STATE;
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x004C or # L
               $self->{nc} == 0x006C) { # l
        ## XML5: <!ELEMENT> not supported.
        $self->{state} = MD_ELEMENT_STATE;
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 2
                            + 1 * ($self->{nc} == -1));
        ## Reconsume.
        $self->{state} = BOGUS_COMMENT_STATE;
        $self->{ct} = {type => COMMENT_TOKEN, data => ''}; ## Will be discarded
        redo A;
      }
    } elsif ($self->{state} == MD_ENTITY_STATE) {
      if ($self->{nc} == [
            undef,
            undef,
            0x0054, # T
            0x0049, # I
            0x0054, # T
          ]->[length $self->{kwd}] or
          $self->{nc} == [
            undef,
            undef,
            0x0074, # t
            0x0069, # i
            0x0074, # t
          ]->[length $self->{kwd}]) {
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 5 and
               ($self->{nc} == 0x0059 or # Y
                $self->{nc} == 0x0079)) { # y
        if ($self->{kwd} ne 'ENTIT' or $self->{nc} == 0x0079) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO: type
                          text => 'ENTITY',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 4);
        }
        $self->{ct} = {type => GENERAL_ENTITY_TOKEN, name => '',
                       line => $self->{line_prev},
                       column => $self->{column_prev} - 6};
        $self->{state} = DOCTYPE_MD_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 1
                            - (length $self->{kwd})
                            + 1 * ($self->{nc} == -1));
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN, data => ''}; ## Will be discarded
        redo A;
      }
    } elsif ($self->{state} == MD_ELEMENT_STATE) {
      if ($self->{nc} == [
           undef,
           undef,
           0x0045, # E
           0x004D, # M
           0x0045, # E
           0x004E, # N
          ]->[length $self->{kwd}] or
          $self->{nc} == [
           undef,
           undef,
           0x0065, # e
           0x006D, # m
           0x0065, # e
           0x006E, # n
          ]->[length $self->{kwd}]) {
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 6 and
               ($self->{nc} == 0x0054 or # T
                $self->{nc} == 0x0074)) { # t
        if ($self->{kwd} ne 'ELEMEN' or $self->{nc} == 0x0074) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO: type
                          text => 'ELEMENT',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 5);
        }
        $self->{ct} = {type => ELEMENT_TOKEN, name => '',
                       line => $self->{line_prev},
                       column => $self->{column_prev} - 7};
        $self->{state} = DOCTYPE_MD_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 1
                            - (length $self->{kwd})
                            + 1 * ($self->{nc} == -1));
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN, data => ''}; ## Will be discarded
        redo A;
      }
    } elsif ($self->{state} == MD_ATTLIST_STATE) {
      if ($self->{nc} == [
           undef,
           0x0054, # T
           0x0054, # T
           0x004C, # L
           0x0049, # I
           0x0053, # S
          ]->[length $self->{kwd}] or
          $self->{nc} == [
           undef,
           0x0074, # t
           0x0074, # t
           0x006C, # l
           0x0069, # i
           0x0073, # s
          ]->[length $self->{kwd}]) {
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 6 and
               ($self->{nc} == 0x0054 or # T
                $self->{nc} == 0x0074)) { # t
        if ($self->{kwd} ne 'ATTLIS' or $self->{nc} == 0x0074) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO: type
                          text => 'ATTLIST',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 5);
        }
        $self->{ct} = {type => ATTLIST_TOKEN, name => '',
                       attrdefs => [],
                       line => $self->{line_prev},
                       column => $self->{column_prev} - 7};
        $self->{state} = DOCTYPE_MD_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 1
                             - (length $self->{kwd})
                             + 1 * ($self->{nc} == -1));
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN, data => ''}; ## Will be discarded
        redo A;
      }
    } elsif ($self->{state} == MD_NOTATION_STATE) {
      if ($self->{nc} == [
           undef,
           0x004F, # O
           0x0054, # T
           0x0041, # A
           0x0054, # T
           0x0049, # I
           0x004F, # O
          ]->[length $self->{kwd}] or
          $self->{nc} == [
           undef,
           0x006F, # o
           0x0074, # t
           0x0061, # a
           0x0074, # t
           0x0069, # i
           0x006F, # o
          ]->[length $self->{kwd}]) {
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 7 and
               ($self->{nc} == 0x004E or # N
                $self->{nc} == 0x006E)) { # n
        if ($self->{kwd} ne 'NOTATIO' or $self->{nc} == 0x006E) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO: type
                          text => 'NOTATION',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 6);
        }
        $self->{ct} = {type => NOTATION_TOKEN, name => '',
                       line => $self->{line_prev},
                       column => $self->{column_prev} - 8};
        $self->{state} = DOCTYPE_MD_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bogus comment',
                        line => $self->{line_prev},
                        column => $self->{column_prev} - 1
                            - (length $self->{kwd})
                            + 1 * ($self->{nc} == -1));
        $self->{state} = BOGUS_COMMENT_STATE;
        ## Reconsume.
        $self->{ct} = {type => COMMENT_TOKEN, data => ''}; ## Will be discarded
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_MD_STATE) {
      ## XML5: "DOCTYPE ENTITY state", "DOCTYPE ATTLIST state", and
      ## "DOCTYPE NOTATION state".

      if ($is_space->{$self->{nc}}) {
        ## XML5: [NOTATION] Switch to the "DOCTYPE NOTATION identifier state".
        $self->{state} = BEFORE_MD_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{ct}->{type} == GENERAL_ENTITY_TOKEN and 
               $self->{nc} == 0x0025) { # %
        ## XML5: Switch to the "DOCTYPE bogus comment state".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before md name'); ## TODO: type
        $self->{state} = DOCTYPE_ENTITY_PARAMETER_BEFORE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        ## Reconsume.
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Switch to the "DOCTYPE bogus comment state".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no md name'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        ## XML5: Switch to the "DOCTYPE bogus comment state".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before md name'); ## TODO: type
        $self->{state} = BEFORE_MD_NAME_STATE;
        redo A;
      }
    } elsif ($self->{state} == BEFORE_MD_NAME_STATE) {
      ## XML5: "DOCTYPE ENTITY parameter state", "DOCTYPE ENTITY type
      ## before state", "DOCTYPE ATTLIST name before state".

      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{ct}->{type} == GENERAL_ENTITY_TOKEN and 
               $self->{nc} == 0x0025) { # %
        $self->{state} = DOCTYPE_ENTITY_PARAMETER_BEFORE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "Anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no md name'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        ## Reconsume.
        redo A;
      } else {
        ## XML5: [ATTLIST] Not defined yet.
        $self->{ct}->{name} .= chr $self->{nc};
        $self->{state} = MD_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ENTITY_PARAMETER_BEFORE_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## XML5: Switch to the "DOCTYPE ENTITY parameter state".
        $self->{ct}->{type} = PARAMETER_ENTITY_TOKEN;
        $self->{state} = BEFORE_MD_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "Anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no md name'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md');
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        ## Reconsume.
        redo A;
      } else {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space after ENTITY percent'); ## TODO: type
        $self->{state} = BOGUS_COMMENT_STATE;
        $self->{ct} = {type => COMMENT_TOKEN, data => ''}; ## Will be discarded
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == MD_NAME_STATE) {
      ## XML5: "DOCTYPE ENTITY name state" and "DOCTYPE ATTLIST name state".
      
      if ($is_space->{$self->{nc}}) {
        if ($self->{ct}->{type} == ATTLIST_TOKEN) {
          $self->{state} = DOCTYPE_ATTLIST_NAME_AFTER_STATE;
        } elsif ($self->{ct}->{type} == ELEMENT_TOKEN) {
          $self->{state} = AFTER_ELEMENT_NAME_STATE;
        } else { # ENTITY/NOTATION
          $self->{state} = AFTER_DOCTYPE_NAME_STATE;
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{ct}->{type} == ATTLIST_TOKEN) {
          #
        } else {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'no md def'); ## TODO: type
        }
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT/ENTITY/ATTLIST/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: [ATTLIST] No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md');
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        ## Reconsume.
        return  ($self->{ct}); # ELEMENT/ENTITY/ATTLIST/NOTATION
        redo A;
      } else {
        ## XML5: [ATTLIST] Not defined yet.
        $self->{ct}->{name} .= chr $self->{nc};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_NAME_AFTER_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        return  ($self->{ct});
        redo A;
      } else {
        ## XML5: Not defined yet.
        $self->{ca} = {name => chr ($self->{nc}), # attrdef
                       tokens => [],
                       line => $self->{line}, column => $self->{column}};
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_ATTRIBUTE_NAME_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_NAME_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr type'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == 0x0028) { # (
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before paren'); ## TODO: type
        $self->{state} = BEFORE_ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } else {
        ## XML5: Not defined yet.
        $self->{ca}->{name} .= chr $self->{nc};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_ATTRIBUTE_NAME_AFTER_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr type'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == 0x0028) { # (
        ## XML5: Same as "anything else".
        $self->{state} = BEFORE_ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        ## XML5: Not defined yet.
        $self->{ca}->{type} = chr $self->{nc};
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_TYPE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_ATTRIBUTE_TYPE_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_TYPE_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0023) { # #
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_BEFORE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr default'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == 0x0028) { # (
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before paren'); ## TODO: type
        $self->{state} = BEFORE_ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        ## XML5: Not defined yet.
        $self->{ca}->{type} .= chr $self->{nc};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_ATTRIBUTE_TYPE_AFTER_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0028) { # (
        ## XML5: Same as "anything else".
        $self->{state} = BEFORE_ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0023) { # #
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_BEFORE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        ## XML5: Same as "anything else".
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        ## XML5: Same as "anything else".
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr default'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        ## XML5: Switch to the "DOCTYPE bogus comment state".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unquoted attr value'); ## TODO: type
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_UNQUOTED_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == BEFORE_ALLOWED_TOKEN_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x007C) { # |
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'empty allowed token'); ## TODO: type
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0029) { # )
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'empty allowed token'); ## TODO: type
        $self->{state} = AFTER_ALLOWED_TOKENS_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed allowed tokens'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        push @{$self->{ca}->{tokens}}, chr $self->{nc};
        $self->{state} = ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == ALLOWED_TOKEN_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = AFTER_ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x007C) { # |
        $self->{state} = BEFORE_ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0029) { # )
        $self->{state} = AFTER_ALLOWED_TOKENS_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed allowed tokens'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        $self->{ca}->{tokens}->[-1] .= chr $self->{nc};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_ALLOWED_TOKEN_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x007C) { # |
        $self->{state} = BEFORE_ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0029) { # )
        $self->{state} = AFTER_ALLOWED_TOKENS_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed allowed tokens'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'space in allowed token', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev});
        $self->{ca}->{tokens}->[-1] .= ' ' . chr $self->{nc};
        $self->{state} = ALLOWED_TOKEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_ALLOWED_TOKENS_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = BEFORE_ATTR_DEFAULT_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0023) { # #
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_BEFORE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr default'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unquoted attr value'); ## TODO: type
        $self->{state} = ATTRIBUTE_VALUE_UNQUOTED_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == BEFORE_ATTR_DEFAULT_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0023) { # #
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_BEFORE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr default'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unquoted attr value'); ## TODO: type
        $self->{state} = ATTRIBUTE_VALUE_UNQUOTED_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_BEFORE_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no default type'); ## TODO: type
        $self->{state} = BOGUS_MD_STATE;
        ## Reconsume.
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        ## XML5: Same as "anything else".
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        ## XML5: Same as "anything else".
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no attr default'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        $self->{ca}->{default} = chr $self->{nc};
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_AFTER_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        ## XML5: Same as "anything else".
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before default value'); ## TODO: type
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        ## XML5: Same as "anything else".
        push @{$self->{ct}->{attrdefs}}, $self->{ca};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        push @{$self->{ct}->{attrdefs}}, $self->{ca};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        $self->{ca}->{default} .= chr $self->{nc};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ATTLIST_ATTRIBUTE_DECLARATION_AFTER_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0022) { # "
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_DOUBLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0027) { # '
        $self->{ca}->{value} = '';
        $self->{state} = ATTRIBUTE_VALUE_SINGLE_QUOTED_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        push @{$self->{ct}->{attrdefs}}, $self->{ca};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST
        redo A;
      } elsif ($self->{nc} == -1) {
        ## XML5: No parse error.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        push @{$self->{ct}->{attrdefs}}, $self->{ca};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE; ## XML5: "Data state".
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct});
        redo A;
      } else {
        ## XML5: Not defined yet.
        if ($self->{ca}->{default} eq 'FIXED') {
          $self->{state} = ATTRIBUTE_VALUE_UNQUOTED_STATE;
        } else {
          push @{$self->{ct}->{attrdefs}}, $self->{ca};
          $self->{state} = DOCTYPE_ATTLIST_NAME_AFTER_STATE;
        }
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == AFTER_ATTLIST_ATTR_VALUE_QUOTED_STATE) {
      if ($is_space->{$self->{nc}} or
          $self->{nc} == -1 or
          $self->{nc} == 0x003E) { # >
        $self->{state} = DOCTYPE_ATTLIST_NAME_AFTER_STATE;
        ## Reconsume.
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no space before attr name'); ## TODO: type
        $self->{state} = DOCTYPE_ATTLIST_NAME_AFTER_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == NDATA_STATE) {
      ## ASCII case-insensitive
      if ($self->{nc} == [
            undef, 
            0x0044, # D
            0x0041, # A
            0x0054, # T
          ]->[length $self->{kwd}] or
          $self->{nc} == [
            undef, 
            0x0064, # d
            0x0061, # a
            0x0074, # t
          ]->[length $self->{kwd}]) {
        
        ## Stay in the state.
        $self->{kwd} .= chr $self->{nc};
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ((length $self->{kwd}) == 4 and
               ($self->{nc} == 0x0041 or # A
                $self->{nc} == 0x0061)) { # a
        if ($self->{kwd} ne 'NDAT' or $self->{nc} == 0x0061) { # a
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'lowercase keyword', ## TODO: type
                          text => 'NDATA',
                          line => $self->{line_prev},
                          column => $self->{column_prev} - 4);
        } else {
          
        }
        $self->{state} = AFTER_NDATA_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after literal', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev} + 1
                            - length $self->{kwd});
        
        $self->{state} = BOGUS_MD_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == AFTER_NDATA_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = BEFORE_NOTATION_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no notation name'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after literal', ## TODO: type
                        line => $self->{line_prev},
                        column => $self->{column_prev} + 1
                            - length $self->{kwd});
        $self->{state} = BOGUS_MD_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == BEFORE_NOTATION_NAME_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no notation name'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY
        redo A;
      } else {
        $self->{ct}->{notation} = chr $self->{nc}; # ENTITY
        $self->{state} = NOTATION_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == NOTATION_NAME_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = AFTER_MD_DEF_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY
        redo A;
      } else {
        $self->{ct}->{notation} .= chr $self->{nc}; # ENTITY
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ENTITY_VALUE_DOUBLE_QUOTED_STATE) {
      if ($self->{nc} == 0x0022) { # "
        $self->{state} = AFTER_MD_DEF_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0026) { # &
        $self->{prev_state} = $self->{state};
        $self->{state} = ENTITY_VALUE_ENTITY_STATE;
        $self->{entity_add} = 0x0022; # "
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
## TODO: %
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed entity value'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        ## Reconsume.
        return  ($self->{ct}); # ENTITY
        redo A;
      } else {
        $self->{ct}->{value} .= chr $self->{nc}; # ENTITY
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == DOCTYPE_ENTITY_VALUE_SINGLE_QUOTED_STATE) {
      if ($self->{nc} == 0x0027) { # '
        $self->{state} = AFTER_MD_DEF_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0026) { # &
        $self->{prev_state} = $self->{state};
        $self->{state} = ENTITY_VALUE_ENTITY_STATE;
        $self->{entity_add} = 0x0027; # '
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
## TODO: %
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed entity value'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        ## Reconsume.
        return  ($self->{ct}); # ENTITY
        redo A;
      } else {
        $self->{ct}->{value} .= chr $self->{nc}; # ENTITY
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == ENTITY_VALUE_ENTITY_STATE) {
      if ($is_space->{$self->{nc}} or
          {
            0x003C => 1, 0x0026 => 1, -1 => 1, # <, &
            $self->{entity_add} => 1,
          }->{$self->{nc}}) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bare ero',
                        line => $self->{line_prev},
                        column => $self->{column_prev}
                            + ($self->{nc} == -1 ? 1 : 0));
        ## Don't consume
        ## Return nothing.
        #
      } elsif ($self->{nc} == 0x0023) { # #
        $self->{ca} = $self->{ct};
        $self->{state} = ENTITY_HASH_STATE;
        $self->{kwd} = '#';
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } else {
        #
      }

      $self->{ct}->{value} .= '&';
      $self->{state} = $self->{prev_state};
      ## Reconsume.
      redo A;
    } elsif ($self->{state} == AFTER_ELEMENT_NAME_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = BEFORE_ELEMENT_CONTENT_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0028) { # (
        $self->{state} = AFTER_CM_GROUP_OPEN_STATE;
        $self->{ct}->{content} = ['('];
        $self->{group_depth} = 1;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'no md def'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } else {
        $self->{ct}->{content} = [chr $self->{nc}];
        $self->{state} = CONTENT_KEYWORD_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == CONTENT_KEYWORD_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = AFTER_MD_DEF_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } else {
        $self->{ct}->{content}->[-1] .= chr $self->{nc}; # ELEMENT
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_CM_GROUP_OPEN_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0028) { # (
        $self->{group_depth}++;
        push @{$self->{ct}->{content}}, chr $self->{nc};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x007C or # |
               $self->{nc} == 0x002C) { # ,
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'empty element name'); ## TODO: type
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0029) { # )
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'empty element name'); ## TODO: type
        push @{$self->{ct}->{content}}, chr $self->{nc};
        $self->{group_depth}--;
        $self->{state} = AFTER_CM_GROUP_CLOSE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed cm group'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } else {
        push @{$self->{ct}->{content}}, chr $self->{nc};
        $self->{state} = CM_ELEMENT_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == CM_ELEMENT_NAME_STATE) {
      if ($is_space->{$self->{nc}}) {
        $self->{state} = AFTER_CM_ELEMENT_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x002A or # *
               $self->{nc} == 0x002B or # +
               $self->{nc} == 0x003F) { # ?
        push @{$self->{ct}->{content}}, chr $self->{nc};
        $self->{state} = AFTER_CM_ELEMENT_NAME_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x007C or # |
               $self->{nc} == 0x002C) { # ,
        push @{$self->{ct}->{content}}, $self->{nc} == 0x007C ? ' | ' : ', ';
        $self->{state} = AFTER_CM_GROUP_OPEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0029) { # )
        $self->{group_depth}--;
        push @{$self->{ct}->{content}}, chr $self->{nc};
        $self->{state} = AFTER_CM_GROUP_CLOSE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed cm group'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } else {
        $self->{ct}->{content}->[-1] .= chr $self->{nc};
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_CM_ELEMENT_NAME_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x007C or # |
               $self->{nc} == 0x002C) { # ,
        push @{$self->{ct}->{content}}, $self->{nc} == 0x007C ? ' | ' : ', ';
        $self->{state} = AFTER_CM_GROUP_OPEN_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0029) { # )
        $self->{group_depth}--;
        push @{$self->{ct}->{content}}, chr $self->{nc};
        $self->{state} = AFTER_CM_GROUP_CLOSE_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed cm group'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'after element name'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = BOGUS_MD_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } elsif ($self->{state} == AFTER_CM_GROUP_CLOSE_STATE) {
      if ($is_space->{$self->{nc}}) {
        if ($self->{group_depth}) {
          $self->{state} = AFTER_CM_ELEMENT_NAME_STATE;
        } else {
          $self->{state} = AFTER_MD_DEF_STATE;
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x002A or # *
               $self->{nc} == 0x002B or # +
               $self->{nc} == 0x003F) { # ?
        push @{$self->{ct}->{content}}, chr $self->{nc};
        if ($self->{group_depth}) {
          $self->{state} = AFTER_CM_ELEMENT_NAME_STATE;
        } else {
          $self->{state} = AFTER_MD_DEF_STATE;
        }
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x0029) { # )
        if ($self->{group_depth}) {
          $self->{group_depth}--;
          push @{$self->{ct}->{content}}, chr $self->{nc};
          ## Stay in the state.
          
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
          redo A;
        } else {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after md def'); ## TODO: type
          $self->{state} = BOGUS_MD_STATE;
          ## Reconsume.
          redo A;
        }
      } elsif ($self->{nc} == 0x003E) { # >
        if ($self->{group_depth}) {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed cm group'); ## TODO: type
          push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        }
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        push @{$self->{ct}->{content}}, (')') x $self->{group_depth};
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ELEMENT
        redo A;
      } else {
        if ($self->{group_depth}) {
          $self->{state} = AFTER_CM_ELEMENT_NAME_STATE;
        } else {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after md def'); ## TODO: type
          $self->{state} = BOGUS_MD_STATE;
        }
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == AFTER_MD_DEF_STATE) {
      if ($is_space->{$self->{nc}}) {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      } elsif ($self->{nc} == 0x003E) { # >
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY/ELEMENT
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unclosed md'); ## TODO: type
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ENTITY/ELEMENT
        redo A;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'string after md def'); ## TODO: type
        $self->{state} = BOGUS_MD_STATE;
        ## Reconsume.
        redo A;
      }
    } elsif ($self->{state} == BOGUS_MD_STATE) {
      if ($self->{nc} == 0x003E) { # >
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        return  ($self->{ct}); # ATTLIST/ENTITY/NOTATION
        redo A;
      } elsif ($self->{nc} == -1) {
        $self->{state} = DOCTYPE_INTERNAL_SUBSET_STATE;
        ## Reconsume.
        return  ($self->{ct}); # ATTLIST/ENTITY/NOTATION
        redo A;
      } else {
        ## Stay in the state.
        
    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      $self->{line_prev} = $self->{line};
      $self->{column_prev} = $self->{column};
      $self->{column}++;
      $self->{nc}
          = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
    } else {
      $self->{set_nc}->($self);
    }
  
        redo A;
      }
    } else {
      die "$0: $self->{state}: Unknown state";
    }
  } # A   

  die "$0: _get_next_token: unexpected case";
} # _get_next_token

1;
## $Date: 2009/09/05 11:31:58 $
                                
