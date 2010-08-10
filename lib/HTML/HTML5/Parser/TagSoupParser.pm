package HTML::HTML5::Parser::TagSoupParser;

# This is a port of the Whatpm::HTML package away from dependencies
# on manakai, and towards CPAN and XML::LibXML.

# caught up to Whatpm::HTML rv cf2c0df8a6dfb50fee923dfb21b14c83f282ccdc, 23/6/2010.

use 5.008001;
use strict;
#use warnings;

our $VERSION='0.101';
use Error qw(:try);

BEGIN
{
	*XML::LibXML::Element::appendTextFromUnicode = sub {
		my ($element, $parser, $text) = @_;
		utf8::encode($text);
		return $element->appendText($text);
	};
}

our $DATA;
sub DATA
{
	my $object = shift;
	my $oaddr  = refaddr($object);
	
	$DATA->{$oaddr} = {}
		unless defined $DATA->{$oaddr};
		
	my ($k, $v) = @_;
	$DATA->{$oaddr}->{$k} = $v
		if (defined $k);
	
	return $DATA->{$oaddr};
}

use HTML::HTML5::Parser::Tokenizer;

## NOTE: This module don't check all HTML5 parse errors; character
## encoding related parse errors are expected to be handled by relevant
## modules.
## Parse errors for control characters that are not allowed in HTML5
## documents, for surrogate code points, and for noncharacter code
## points, as well as U+FFFD substitions for characters whose code points
## is higher than U+10FFFF may be detected by combining the parser with
## the checker implemented by HTML::HTML5::Parser::Charset::UnicodeChecker (for its
## usage example, see |t/HTML-tree.t| in the Whatpm package or the 
## WebHACC::Language::HTML module in the WebHACC package).

## ISSUE:
## var doc = implementation.createDocument (null, null, null);
## doc.write ('');
## alert (doc.compatMode);

use Scalar::Util qw(refaddr);
require IO::Handle;

## Namespace URLs

sub HTML_NS ()  { q<http://www.w3.org/1999/xhtml> }
sub MML_NS ()   { q<http://www.w3.org/1998/Math/MathML> }
sub SVG_NS ()   { q<http://www.w3.org/2000/svg> }
sub XLINK_NS () { q<http://www.w3.org/1999/xlink> }
sub XML_NS ()   { q<http://www.w3.org/XML/1998/namespace> }
sub XMLNS_NS () { q<http://www.w3.org/2000/xmlns/> }

## Element categories

## Bits 12-15
sub SPECIAL_EL () { 0b1_000000000000000 }
sub SCOPING_EL () { 0b1_00000000000000 }
sub FORMATTING_EL () { 0b1_0000000000000 }
sub PHRASING_EL () { 0b1_000000000000 }

## Bits 10-11
#sub FOREIGN_EL () { 0b1_00000000000 } # see HTML::HTML5::Parser::Tokenizer
sub FOREIGN_FLOW_CONTENT_EL () { 0b1_0000000000 }

## Bits 6-9
sub TABLE_SCOPING_EL () { 0b1_000000000 }
sub TABLE_ROWS_SCOPING_EL () { 0b1_00000000 }
sub TABLE_ROW_SCOPING_EL () { 0b1_0000000 }
sub TABLE_ROWS_EL () { 0b1_000000 }

## Bit 5
sub ADDRESS_DIV_P_EL () { 0b1_00000 }

## NOTE: Used in </body> and EOF algorithms.
## Bit 4
sub ALL_END_TAG_OPTIONAL_EL () { 0b1_0000 }

## NOTE: Used in "generate implied end tags" algorithm.
## NOTE: There is a code where a modified version of
## END_TAG_OPTIONAL_EL is used in "generate implied end tags"
## implementation (search for the algorithm name).
## Bit 3
sub END_TAG_OPTIONAL_EL () { 0b1_000 }

## Bits 0-2

sub MISC_SPECIAL_EL () { SPECIAL_EL | 0b000 }
sub FORM_EL () { SPECIAL_EL | 0b001 }
sub FRAMESET_EL () { SPECIAL_EL | 0b010 }
sub HEADING_EL () { SPECIAL_EL | 0b011 }
sub SELECT_EL () { SPECIAL_EL | 0b100 }
sub SCRIPT_EL () { SPECIAL_EL | 0b101 }

sub ADDRESS_DIV_EL () { SPECIAL_EL | ADDRESS_DIV_P_EL | 0b001 }
sub BODY_EL () { SPECIAL_EL | ALL_END_TAG_OPTIONAL_EL | 0b001 }

sub DTDD_EL () {
  SPECIAL_EL |
  END_TAG_OPTIONAL_EL |
  ALL_END_TAG_OPTIONAL_EL |
  0b010
}
sub LI_EL () {
  SPECIAL_EL |
  END_TAG_OPTIONAL_EL |
  ALL_END_TAG_OPTIONAL_EL |
  0b100
}
sub P_EL () {
  SPECIAL_EL |
  ADDRESS_DIV_P_EL |
  END_TAG_OPTIONAL_EL |
  ALL_END_TAG_OPTIONAL_EL |
  0b001
}

sub TABLE_ROW_EL () {
  SPECIAL_EL |
  TABLE_ROWS_EL |
  TABLE_ROW_SCOPING_EL |
  ALL_END_TAG_OPTIONAL_EL |
  0b001
}
sub TABLE_ROW_GROUP_EL () {
  SPECIAL_EL |
  TABLE_ROWS_EL |
  TABLE_ROWS_SCOPING_EL |
  ALL_END_TAG_OPTIONAL_EL |
  0b001
}

sub MISC_SCOPING_EL () { SCOPING_EL | 0b000 }
sub BUTTON_EL () { SCOPING_EL | 0b001 }
sub CAPTION_EL () { SCOPING_EL | 0b010 }
sub HTML_EL () {
  SCOPING_EL |
  TABLE_SCOPING_EL |
  TABLE_ROWS_SCOPING_EL |
  TABLE_ROW_SCOPING_EL |
  ALL_END_TAG_OPTIONAL_EL |
  0b001
}
sub TABLE_EL () {
  SCOPING_EL |
  TABLE_ROWS_EL |
  TABLE_SCOPING_EL |
  0b001
}
sub TABLE_CELL_EL () {
  SCOPING_EL |
  TABLE_ROW_SCOPING_EL |
  ALL_END_TAG_OPTIONAL_EL |
  0b001
}

sub MISC_FORMATTING_EL () { FORMATTING_EL | 0b000 }
sub A_EL () { FORMATTING_EL | 0b001 }
sub NOBR_EL () { FORMATTING_EL | 0b010 }

sub RUBY_EL () { PHRASING_EL | 0b001 }

## ISSUE: ALL_END_TAG_OPTIONAL_EL?
sub OPTGROUP_EL () { PHRASING_EL | END_TAG_OPTIONAL_EL | 0b001 }
sub OPTION_EL () { PHRASING_EL | END_TAG_OPTIONAL_EL | 0b010 }
sub RUBY_COMPONENT_EL () { PHRASING_EL | END_TAG_OPTIONAL_EL | 0b100 }

sub MML_AXML_EL () { PHRASING_EL | FOREIGN_EL | 0b001 }

my $el_category = {
  a => A_EL,
  address => ADDRESS_DIV_EL,
  applet => MISC_SCOPING_EL,
  area => MISC_SPECIAL_EL,
  article => MISC_SPECIAL_EL,
  aside => MISC_SPECIAL_EL,
  b => FORMATTING_EL,
  base => MISC_SPECIAL_EL,
  basefont => MISC_SPECIAL_EL,
  bgsound => MISC_SPECIAL_EL,
  big => FORMATTING_EL,
  blockquote => MISC_SPECIAL_EL,
  body => BODY_EL,
  br => MISC_SPECIAL_EL,
  button => BUTTON_EL,
  caption => CAPTION_EL,
  center => MISC_SPECIAL_EL,
  code => FORMATTING_EL,
  col => MISC_SPECIAL_EL,
  colgroup => MISC_SPECIAL_EL,
  command => MISC_SPECIAL_EL,
  #datagrid => MISC_SPECIAL_EL,
  dd => DTDD_EL,
  details => MISC_SPECIAL_EL,
  dir => MISC_SPECIAL_EL,
  div => ADDRESS_DIV_EL,
  dl => MISC_SPECIAL_EL,
  dt => DTDD_EL,
  em => FORMATTING_EL,
  embed => MISC_SPECIAL_EL,
  fieldset => MISC_SPECIAL_EL,
  figure => MISC_SPECIAL_EL,
  font => FORMATTING_EL,
  footer => MISC_SPECIAL_EL,
  form => FORM_EL,
  frame => MISC_SPECIAL_EL,
  frameset => FRAMESET_EL,
  h1 => HEADING_EL,
  h2 => HEADING_EL,
  h3 => HEADING_EL,
  h4 => HEADING_EL,
  h5 => HEADING_EL,
  h6 => HEADING_EL,
  head => MISC_SPECIAL_EL,
  header => MISC_SPECIAL_EL,
  hgroup => MISC_SPECIAL_EL,
  hr => MISC_SPECIAL_EL,
  html => HTML_EL,
  i => FORMATTING_EL,
  iframe => MISC_SPECIAL_EL,
  img => MISC_SPECIAL_EL,
  #image => MISC_SPECIAL_EL, ## NOTE: Commented out in the spec.
  input => MISC_SPECIAL_EL,
  isindex => MISC_SPECIAL_EL,
  ## XXX keygen? (Whether a void element is in Special or not does not
  ## affect to the processing, however.)
  li => LI_EL,
  link => MISC_SPECIAL_EL,
  listing => MISC_SPECIAL_EL,
  marquee => MISC_SCOPING_EL,
  menu => MISC_SPECIAL_EL,
  meta => MISC_SPECIAL_EL,
  nav => MISC_SPECIAL_EL,
  nobr => NOBR_EL,
  noembed => MISC_SPECIAL_EL,
  noframes => MISC_SPECIAL_EL,
  noscript => MISC_SPECIAL_EL,
  object => MISC_SCOPING_EL,
  ol => MISC_SPECIAL_EL,
  optgroup => OPTGROUP_EL,
  option => OPTION_EL,
  p => P_EL,
  param => MISC_SPECIAL_EL,
  plaintext => MISC_SPECIAL_EL,
  pre => MISC_SPECIAL_EL,
  rp => RUBY_COMPONENT_EL,
  rt => RUBY_COMPONENT_EL,
  ruby => RUBY_EL,
  s => FORMATTING_EL,
  script => MISC_SPECIAL_EL,
  select => SELECT_EL,
  section => MISC_SPECIAL_EL,
  small => FORMATTING_EL,
  strike => FORMATTING_EL,
  strong => FORMATTING_EL,
  style => MISC_SPECIAL_EL,
  table => TABLE_EL,
  tbody => TABLE_ROW_GROUP_EL,
  td => TABLE_CELL_EL,
  textarea => MISC_SPECIAL_EL,
  tfoot => TABLE_ROW_GROUP_EL,
  th => TABLE_CELL_EL,
  thead => TABLE_ROW_GROUP_EL,
  title => MISC_SPECIAL_EL,
  tr => TABLE_ROW_EL,
  tt => FORMATTING_EL,
  u => FORMATTING_EL,
  ul => MISC_SPECIAL_EL,
  wbr => MISC_SPECIAL_EL,
  xmp => MISC_SPECIAL_EL,
};

my $el_category_f = {
  (MML_NS) => {
    'annotation-xml' => MML_AXML_EL,
    mi => FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
    mo => FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
    mn => FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
    ms => FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
    mtext => FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
  },
  (SVG_NS) => {
    foreignObject => SCOPING_EL | FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
    desc => FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
    title => FOREIGN_EL | FOREIGN_FLOW_CONTENT_EL,
  },
  ## NOTE: In addition, FOREIGN_EL is set to non-HTML elements.
};

my $svg_attr_name = {
  attributename => 'attributeName',
  attributetype => 'attributeType',
  basefrequency => 'baseFrequency',
  baseprofile => 'baseProfile',
  calcmode => 'calcMode',
  clippathunits => 'clipPathUnits',
  contentscripttype => 'contentScriptType',
  contentstyletype => 'contentStyleType',
  diffuseconstant => 'diffuseConstant',
  edgemode => 'edgeMode',
  externalresourcesrequired => 'externalResourcesRequired',
  filterres => 'filterRes',
  filterunits => 'filterUnits',
  glyphref => 'glyphRef',
  gradienttransform => 'gradientTransform',
  gradientunits => 'gradientUnits',
  kernelmatrix => 'kernelMatrix',
  kernelunitlength => 'kernelUnitLength',
  keypoints => 'keyPoints',
  keysplines => 'keySplines',
  keytimes => 'keyTimes',
  lengthadjust => 'lengthAdjust',
  limitingconeangle => 'limitingConeAngle',
  markerheight => 'markerHeight',
  markerunits => 'markerUnits',
  markerwidth => 'markerWidth',
  maskcontentunits => 'maskContentUnits',
  maskunits => 'maskUnits',
  numoctaves => 'numOctaves',
  pathlength => 'pathLength',
  patterncontentunits => 'patternContentUnits',
  patterntransform => 'patternTransform',
  patternunits => 'patternUnits',
  pointsatx => 'pointsAtX',
  pointsaty => 'pointsAtY',
  pointsatz => 'pointsAtZ',
  preservealpha => 'preserveAlpha',
  preserveaspectratio => 'preserveAspectRatio',
  primitiveunits => 'primitiveUnits',
  refx => 'refX',
  refy => 'refY',
  repeatcount => 'repeatCount',
  repeatdur => 'repeatDur',
  requiredextensions => 'requiredExtensions',
  requiredfeatures => 'requiredFeatures',
  specularconstant => 'specularConstant',
  specularexponent => 'specularExponent',
  spreadmethod => 'spreadMethod',
  startoffset => 'startOffset',
  stddeviation => 'stdDeviation',
  stitchtiles => 'stitchTiles',
  surfacescale => 'surfaceScale',
  systemlanguage => 'systemLanguage',
  tablevalues => 'tableValues',
  targetx => 'targetX',
  targety => 'targetY',
  textlength => 'textLength',
  viewbox => 'viewBox',
  viewtarget => 'viewTarget',
  xchannelselector => 'xChannelSelector',
  ychannelselector => 'yChannelSelector',
  zoomandpan => 'zoomAndPan',
};

my $foreign_attr_xname = {
  'xlink:actuate' => [(XLINK_NS), ['xlink', 'actuate']],
  'xlink:arcrole' => [(XLINK_NS), ['xlink', 'arcrole']],
  'xlink:href' => [(XLINK_NS), ['xlink', 'href']],
  'xlink:role' => [(XLINK_NS), ['xlink', 'role']],
  'xlink:show' => [(XLINK_NS), ['xlink', 'show']],
  'xlink:title' => [(XLINK_NS), ['xlink', 'title']],
  'xlink:type' => [(XLINK_NS), ['xlink', 'type']],
  'xml:base' => [(XML_NS), ['xml', 'base']],
  'xml:lang' => [(XML_NS), ['xml', 'lang']],
  'xml:space' => [(XML_NS), ['xml', 'space']],
  'xmlns' => [(XMLNS_NS), [undef, 'xmlns']],
  'xmlns:xlink' => [(XMLNS_NS), ['xmlns', 'xlink']],
};

## TODO: Invoke the reset algorithm when a resettable element is
## created (cf. HTML5 revision 2259).

sub parse_byte_string ($$$$;$) {
  my $self = shift;
  my $charset_name = shift;
  open my $input, '<', ref $_[0] ? $_[0] : \($_[0]);
  return $self->parse_byte_stream ($charset_name, $input, @_[1..$#_]);
} # parse_byte_string

sub parse_byte_stream ($$$$;$$) {
  # my ($self, $charset_name, $byte_stream, $doc, $onerror, $get_wrapper) = @_;
  my $self = ref $_[0] ? shift : shift->new;
  my $charset_name = shift;
  my $byte_stream = $_[0];

  my $onerror = $_[2] || sub {
    my (%opt) = @_;
    warn "Parse error ($opt{type})\n";
  };
  $self->{parse_error} = $onerror; # updated later by parse_char_string

  my $get_wrapper = $_[3] || sub ($) {
    return $_[0]; # $_[0] = byte stream handle, returned = arg to char handle
  };

  ## HTML5 encoding sniffing algorithm
  require HTML::HTML5::Parser::Charset::Info;
  my $charset;
  my $buffer;
  my ($char_stream, $e_status);

  SNIFFING: {
    ## NOTE: By setting |allow_fallback| option true when the
    ## |get_decode_handle| method is invoked, we ignore what the HTML5
    ## spec requires, i.e. unsupported encoding should be ignored.
      ## TODO: We should not do this unless the parser is invoked
      ## in the conformance checking mode, in which this behavior
      ## would be useful.

    ## Step 1
    if (defined $charset_name) {
      $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ($charset_name);
          ## TODO: Is this ok?  Transfer protocol's parameter should be
          ## interpreted in its semantics?

      ($char_stream, $e_status) = $charset->get_decode_handle
          ($byte_stream, allow_error_reporting => 1,
           allow_fallback => 1);
      if ($char_stream) {
        $self->{confident} = 1;
        last SNIFFING;
      } else {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'charset:not supported',
                        layer => 'encode',
                        line => 1, column => 1,
                        value => $charset_name,
                        level => $self->{level}->{uncertain});
      }
    }

    ## Step 2
    my $byte_buffer = '';
    for (1..1024) {
      my $char = $byte_stream->getc;
      last unless defined $char;
      $byte_buffer .= $char;
    } ## TODO: timeout

    ## Step 3
    if ($byte_buffer =~ /^\xFE\xFF/) {
      $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ('utf-16be');
      ($char_stream, $e_status) = $charset->get_decode_handle
          ($byte_stream, allow_error_reporting => 1,
           allow_fallback => 1, byte_buffer => \$byte_buffer);
      $self->{confident} = 1;
      last SNIFFING;
    } elsif ($byte_buffer =~ /^\xFF\xFE/) {
      $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ('utf-16le');
      ($char_stream, $e_status) = $charset->get_decode_handle
          ($byte_stream, allow_error_reporting => 1,
           allow_fallback => 1, byte_buffer => \$byte_buffer);
      $self->{confident} = 1;
      last SNIFFING;
    } elsif ($byte_buffer =~ /^\xEF\xBB\xBF/) {
      $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ('utf-8');
      ($char_stream, $e_status) = $charset->get_decode_handle
          ($byte_stream, allow_error_reporting => 1,
           allow_fallback => 1, byte_buffer => \$byte_buffer);
      $self->{confident} = 1;
      last SNIFFING;
    }

    ## Step 4
    ## TODO: <meta charset>

    ## Step 5
    ## TODO: from history

    ## Step 6
    require HTML::HTML5::Parser::Charset::UniversalCharDet;
    $charset_name = HTML::HTML5::Parser::Charset::UniversalCharDet->detect_byte_string($byte_buffer)
		if $byte_buffer;
    if (defined $charset_name) {
      $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ($charset_name);

      require HTML::HTML5::Parser::Charset::DecodeHandle;
      $buffer = HTML::HTML5::Parser::Charset::DecodeHandle::ByteBuffer->new
          ($byte_stream);
      ($char_stream, $e_status) = $charset->get_decode_handle
          ($buffer, allow_error_reporting => 1,
           allow_fallback => 1, byte_buffer => \$byte_buffer);
      if ($char_stream) {
        $buffer->{buffer} = $byte_buffer;
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'sniffing:chardet',
                        text => $charset_name,
                        level => $self->{level}->{info},
                        layer => 'encode',
                        line => 1, column => 1);
        $self->{confident} = 0;
        last SNIFFING;
      }
    }

    ## Step 7: default
    ## TODO: Make this configurable.
    $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ('windows-1252');
        ## NOTE: We choose |windows-1252| here, since |utf-8| should be 
        ## detectable in the step 6.
    require HTML::HTML5::Parser::Charset::DecodeHandle;
    $buffer = HTML::HTML5::Parser::Charset::DecodeHandle::ByteBuffer->new
        ($byte_stream);
    ($char_stream, $e_status)
        = $charset->get_decode_handle ($buffer,
                                       allow_error_reporting => 1,
                                       allow_fallback => 1,
                                       byte_buffer => \$byte_buffer);
    $buffer->{buffer} = $byte_buffer;
    $self->{parse_error}->(level => $self->{level}->{must}, type => 'sniffing:default',
                    text => 'windows-1252',
                    level => $self->{level}->{info},
                    line => 1, column => 1,
                    layer => 'encode');
    $self->{confident} = 0;
  } # SNIFFING

  if ($e_status & HTML::HTML5::Parser::Charset::Info::FALLBACK_ENCODING_IMPL ()) {
    $self->{input_encoding} = $charset->get_iana_name; ## TODO: Should we set actual charset decoder's encoding name?
    $self->{parse_error}->(level => $self->{level}->{must}, type => 'chardecode:fallback',
                    #text => $self->{input_encoding},
                    level => $self->{level}->{uncertain},
                    line => 1, column => 1,
                    layer => 'encode');
  } elsif (not ($e_status &
                HTML::HTML5::Parser::Charset::Info::ERROR_REPORTING_ENCODING_IMPL ())) {
    $self->{input_encoding} = $charset->get_iana_name;
    $self->{parse_error}->(level => $self->{level}->{must}, type => 'chardecode:no error',
                    text => $self->{input_encoding},
                    level => $self->{level}->{uncertain},
                    line => 1, column => 1,
                    layer => 'encode');
  } else {
    $self->{input_encoding} = $charset->get_iana_name;
  }

  $self->{change_encoding} = sub {
    my $self = shift;
    $charset_name = shift;
    my $token = shift;

    $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ($charset_name);
    ($char_stream, $e_status) = $charset->get_decode_handle
        ($byte_stream, allow_error_reporting => 1, allow_fallback => 1,
         byte_buffer => \ $buffer->{buffer});
    
    if ($char_stream) { # if supported
      ## "Change the encoding" algorithm:
      
      ## Step 1
      if (defined $self->{input_encoding} and
          $self->{input_encoding} eq $charset_name) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'charset label:matching',
                        text => $charset_name,
                        level => $self->{level}->{info});
        $self->{confident} = 1;
        return;
      }

      ## Step 2 (HTML5 revision 3205)
      if (defined $self->{input_encoding} and
          HTML::HTML5::Parser::Charset::Info->get_by_html_name ($self->{input_encoding})
          ->{category} & HTML::HTML5::Parser::Charset::Info::CHARSET_CATEGORY_UTF16 ()) {
        $self->{confident} = 1;
        return;
      }

      ## Step 3
      if ($charset->{category} &
          HTML::HTML5::Parser::Charset::Info::CHARSET_CATEGORY_UTF16 ()) {
        $charset = HTML::HTML5::Parser::Charset::Info->get_by_html_name ('utf-8');
        ($char_stream, $e_status) = $charset->get_decode_handle
            ($byte_stream,
             byte_buffer => \ $buffer->{buffer});
      }
      $charset_name = $charset->get_iana_name;

      $self->{parse_error}->(level => $self->{level}->{must}, type => 'charset label detected',
                      text => $self->{input_encoding},
                      value => $charset_name,
                      level => $self->{level}->{warn},
                      token => $token);
      
      ## Step 4
      # if (can) {
        ## change the encoding on the fly.
        #$self->{confident} = 1;
        #return;
      # }
      
      ## Step 5
      throw HTML::HTML5::Parser::TagSoupParser::RestartParser ();
    }
  }; # $self->{change_encoding}

  my $char_onerror = sub {
    my (undef, $type, %opt) = @_;
    $self->{parse_error}->(level => $self->{level}->{must}, layer => 'encode',
                    line => $self->{line}, column => $self->{column} + 1,
                    %opt, type => $type);
    if ($opt{octets}) {
      ${$opt{octets}} = "\x{FFFD}"; # relacement character
    }
  };

  my $wrapped_char_stream = $get_wrapper->($char_stream);
  $wrapped_char_stream->onerror ($char_onerror);

  my @args = ($_[1], $_[2]); # $doc, $onerror - $get_wrapper = undef;
  my $return;
  try {
    $return = $self->parse_char_stream ($wrapped_char_stream, @args);  
  } catch HTML::HTML5::Parser::TagSoupParser::RestartParser with {
    ## NOTE: Invoked after {change_encoding}.

    if ($e_status & HTML::HTML5::Parser::Charset::Info::FALLBACK_ENCODING_IMPL ()) {
      $self->{input_encoding} = $charset->get_iana_name; ## TODO: Should we set actual charset decoder's encoding name?
      $self->{parse_error}->(level => $self->{level}->{must}, type => 'chardecode:fallback',
                      level => $self->{level}->{uncertain},
                      #text => $self->{input_encoding},
                      line => 1, column => 1,
                      layer => 'encode');
    } elsif (not ($e_status &
                  HTML::HTML5::Parser::Charset::Info::ERROR_REPORTING_ENCODING_IMPL ())) {
      $self->{input_encoding} = $charset->get_iana_name;
      $self->{parse_error}->(level => $self->{level}->{must}, type => 'chardecode:no error',
                      text => $self->{input_encoding},
                      level => $self->{level}->{uncertain},
                      line => 1, column => 1,
                      layer => 'encode');
    } else {
      $self->{input_encoding} = $charset->get_iana_name;
    }
    $self->{confident} = 1;

    $wrapped_char_stream = $get_wrapper->($char_stream);
    $wrapped_char_stream->onerror ($char_onerror);

    $return = $self->parse_char_stream ($wrapped_char_stream, @args);
  };
  return $return;
} # parse_byte_stream

## NOTE: HTML5 spec says that the encoding layer MUST NOT strip BOM
## and the HTML layer MUST ignore it.  However, we does strip BOM in
## the encoding layer and the HTML layer does not ignore any U+FEFF,
## because the core part of our HTML parser expects a string of character,
## not a string of bytes or code units or anything which might contain a BOM.
## Therefore, any parser interface that accepts a string of bytes,
## such as |parse_byte_string| in this module, must ensure that it does
## strip the BOM and never strip any ZWNBSP.

sub parse_char_string ($$$;$$) {
  #my ($self, $s, $doc, $onerror, $get_wrapper) = @_;
  my $self = shift;
  my $s = ref $_[0] ? $_[0] : \($_[0]);
  require HTML::HTML5::Parser::Charset::DecodeHandle;
  my $input = HTML::HTML5::Parser::Charset::DecodeHandle::CharString->new ($s);
  return $self->parse_char_stream ($input, @_[1..$#_]);
} # parse_char_string
*parse_string = \&parse_char_string; ## NOTE: Alias for backward compatibility.

sub parse_char_stream ($$$;$$) {
  my $self = ref $_[0] ? shift : shift->new;
  my $input = $_[0];
  my $doc = $self->{document} = $_[1];
  $self->{document}->removeChildNodes;

  ## NOTE: |set_inner_html| copies most of this method's code

  ## Confidence: irrelevant.
  $self->{confident} = 1 unless exists $self->{confident};

  $self->{document}->setEncoding($self->{input_encoding})
      if defined $self->{input_encoding};
## TODO: |{input_encoding}| is needless?

  $self->{line_prev} = $self->{line} = 1;
  $self->{column_prev} = -1;
  $self->{column} = 0;
  $self->{set_nc} = sub {
    my $self = shift;

    my $char = '';
    if (defined $self->{next_nc}) {
      $char = $self->{next_nc};
      delete $self->{next_nc};
      $self->{nc} = ord $char;
    } else {
      $self->{char_buffer} = '';
      $self->{char_buffer_pos} = 0;

      my $count = $input->manakai_read_until
         ($self->{char_buffer}, qr/[^\x00\x0A\x0D\x{D800}-\x{DFFF}]/, $self->{char_buffer_pos});
      if ($count) {
        $self->{line_prev} = $self->{line};
        $self->{column_prev} = $self->{column};
        $self->{column}++;
        $self->{nc}
            = ord substr ($self->{char_buffer}, $self->{char_buffer_pos}++, 1);
        return;
      }

      if ($input->read ($char, 1)) {
        $self->{nc} = ord $char;
      } else {
        $self->{nc} = -1;
        return;
      }
    }

    ($self->{line_prev}, $self->{column_prev})
        = ($self->{line}, $self->{column});
    $self->{column}++;
    
    if ($self->{nc} == 0x000A) { # LF
      
      $self->{line}++;
      $self->{column} = 0;
    } elsif ($self->{nc} == 0x000D) { # CR
      
## TODO: support for abort/streaming
      my $next = '';
      if ($input->read ($next, 1) and $next ne "\x0A") {
        $self->{next_nc} = $next;
      }
      $self->{nc} = 0x000A; # LF # MUST
      $self->{line}++;
      $self->{column} = 0;
    } elsif ($self->{nc} == 0x0000) { # NULL
      
      $self->{parse_error}->(level => $self->{level}->{must}, type => 'NULL');
      $self->{nc} = 0xFFFD; # REPLACEMENT CHARACTER # MUST
    } elsif (0xD800 <= $self->{nc} and $self->{nc} <= 0xDFFF) {
      
      $self->{parse_error}->(level => $self->{level}->{must}, type => 'surrogate'); ## XXX documentation
      $self->{nc} = 0xFFFD; # REPLACEMENT CHARACTER # MUST
     }
  };

  $self->{read_until} = sub {
    #my ($scalar, $specials_range, $offset) = @_;
    return 0 if defined $self->{next_nc};

    my $pattern = qr/[^$_[1]\x00\x0A\x0D\x{D800}-\x{DFFF}]/;
    my $offset = $_[2] || 0;

    if ($self->{char_buffer_pos} < length $self->{char_buffer}) {
      pos ($self->{char_buffer}) = $self->{char_buffer_pos};
      if ($self->{char_buffer} =~ /\G(?>$pattern)+/) {
        substr ($_[0], $offset)
            = substr ($self->{char_buffer}, $-[0], $+[0] - $-[0]);
        my $count = $+[0] - $-[0];
        if ($count) {
          $self->{column} += $count;
          $self->{char_buffer_pos} += $count;
          $self->{line_prev} = $self->{line};
          $self->{column_prev} = $self->{column} - 1;
          $self->{nc} = -1;
        }
        return $count;
      } else {
        return 0;
      }
    } else {
      my $count = $input->manakai_read_until ($_[0], $pattern, $_[2]);
      if ($count) {
        $self->{column} += $count;
        $self->{line_prev} = $self->{line};
        $self->{column_prev} = $self->{column} - 1;
        $self->{nc} = -1;
      }
      return $count;
    }
  }; # $self->{read_until}

  my $onerror = $_[2] || sub {
    my (%opt) = @_;
    my $line = $opt{token} ? $opt{token}->{line} : $opt{line};
    my $column = $opt{token} ? $opt{token}->{column} : $opt{column};
    warn "Parse error ($opt{type}) at line $line column $column\n";
  };
  $self->{parse_error} = sub {
    $onerror->(line => $self->{line}, column => $self->{column}, @_);
  };

  my $char_onerror = sub {
    my (undef, $type, %opt) = @_;
    $self->{parse_error}->(level => $self->{level}->{must}, layer => 'encode',
                    line => $self->{line}, column => $self->{column} + 1,
                    %opt, type => $type);
  }; # $char_onerror

  if ($_[3]) {
    $input = $_[3]->($input);
    $input->onerror ($char_onerror);
  } else {
    $input->onerror ($char_onerror) unless defined $input->onerror;
  }

  $self->_initialize_tokenizer;
  $self->_initialize_tree_constructor;
  $self->_construct_tree;
  $self->_terminate_tree_constructor;

  ## Remove self-references
  delete $self->{set_nc}; 
  delete $self->{read_until}; 
  delete $self->{parse_error}; 
  delete $self->{document}; 

  return $doc;
} # parse_char_stream

sub new ($) {
  my $class = shift;
  my $self = bless {
    level => {
      must => 'm',
      should => 's',
      obsconforming => 's',
      warn => 'w',
      info => 'i',
      uncertain => 'u',
    },
  }, $class;
  $self->{set_nc} = sub {
    $self->{nc} = -1;
  };
  $self->{parse_error} = sub {
    # 
  };
  $self->{change_encoding} = sub {
    # if ($_[0] is a supported encoding) {
    #   run "change the encoding" algorithm;
    #   throw Whatpm::HTML::RestartParser (charset => $new_encoding);
    # }
  };
  $self->{application_cache_selection} = sub {
    #
  };
  return $self;
} # new

## Insertion modes

sub AFTER_HTML_IMS () { 0b100 }
sub HEAD_IMS ()       { 0b1000 }
sub BODY_IMS ()       { 0b10000 }
sub BODY_TABLE_IMS () { 0b100000 }
sub TABLE_IMS ()      { 0b1000000 }
sub ROW_IMS ()        { 0b10000000 }
sub BODY_AFTER_IMS () { 0b100000000 }
sub FRAME_IMS ()      { 0b1000000000 }
sub SELECT_IMS ()     { 0b10000000000 }
#sub IN_FOREIGN_CONTENT_IM () { 0b100000000000 } # see HTML::HTML5::Parser::Tokenizer
    ## NOTE: "in foreign content" insertion mode is special; it is combined
    ## with the secondary insertion mode.  In this parser, they are stored
    ## together in the bit-or'ed form.
sub IN_CDATA_RCDATA_IM () { 0b1000000000000 }
    ## NOTE: "in CDATA/RCDATA" insertion mode is also special; it is
    ## combined with the original insertion mode.  In thie parser,
    ## they are stored together in the bit-or'ed form.

sub IM_MASK () { 0b11111111111 }

## NOTE: "initial" and "before html" insertion modes have no constants.

## NOTE: "after after body" insertion mode.
sub AFTER_HTML_BODY_IM () { AFTER_HTML_IMS | BODY_AFTER_IMS }

## NOTE: "after after frameset" insertion mode.
sub AFTER_HTML_FRAMESET_IM () { AFTER_HTML_IMS | FRAME_IMS }

sub IN_HEAD_IM () { HEAD_IMS | 0b00 }
sub IN_HEAD_NOSCRIPT_IM () { HEAD_IMS | 0b01 }
sub AFTER_HEAD_IM () { HEAD_IMS | 0b10 }
sub BEFORE_HEAD_IM () { HEAD_IMS | 0b11 }
sub IN_BODY_IM () { BODY_IMS }
sub IN_CELL_IM () { BODY_IMS | BODY_TABLE_IMS | 0b01 }
sub IN_CAPTION_IM () { BODY_IMS | BODY_TABLE_IMS | 0b10 }
sub IN_ROW_IM () { TABLE_IMS | ROW_IMS | 0b01 }
sub IN_TABLE_BODY_IM () { TABLE_IMS | ROW_IMS | 0b10 }
sub IN_TABLE_IM () { TABLE_IMS }
sub AFTER_BODY_IM () { BODY_AFTER_IMS }
sub IN_FRAMESET_IM () { FRAME_IMS | 0b01 }
sub AFTER_FRAMESET_IM () { FRAME_IMS | 0b10 }
sub IN_SELECT_IM () { SELECT_IMS | 0b01 }
sub IN_SELECT_IN_TABLE_IM () { SELECT_IMS | 0b10 }
sub IN_COLUMN_GROUP_IM () { 0b10 }

sub _initialize_tree_constructor ($) {
  my $self = shift;
  ## NOTE: $self->{document} MUST be specified before this method is called
  DATA($self->{document})->{strict_error_checking} = 0;
  ## TODO: Turn mutation events off # MUST
  ## TODO: Turn loose Document option (manakai extension) on
  DATA($self->{document})->{manakai_is_html} = 1; # MUST
  DATA($self->{document})->{manakai_source_line} = 1;
  DATA($self->{document})->{manakai_source_column} = 1;

  $self->{frameset_ok} = 1;
} # _initialize_tree_constructor

sub _terminate_tree_constructor ($) {
  my $self = shift;
  DATA($self->{document}, strict_error_checking => 1);
  ## TODO: Turn mutation events on
} # _terminate_tree_constructor

## ISSUE: Should appendChild (for example) in script executed in tree construction stage fire mutation events?

{ # tree construction stage
  my $token;

sub _construct_tree ($) {
  my ($self) = @_;

  ## When an interactive UA render the $self->{document} available
  ## to the user, or when it begin accepting user input, are
  ## not defined.
  
  $self->{insertion_mode} = 0; # dummy
  $token = $self->_get_next_token;

  undef $self->{form_element};
  undef $self->{head_element};
  undef $self->{head_element_inserted};
  $self->{open_elements} = [];
  undef $self->{inner_html_node};
  undef $self->{ignore_newline};

  ## NOTE: The "initial" insertion mode.
  $self->_tree_construction_initial; # MUST

  ## NOTE: The "before html" insertion mode.
  $self->_tree_construction_root_element;
  $self->{insertion_mode} = BEFORE_HEAD_IM;

  ## NOTE: The "before head" insertion mode and so on.
  $self->_tree_construction_main;
} # _construct_tree

sub _tree_construction_initial ($) {
  my $self = shift;

  ## NOTE: "initial" insertion mode

  INITIAL: {
    if ($token->{type} == DOCTYPE_TOKEN) {
      ## NOTE: Conformance checkers MAY, instead of reporting "not
      ## HTML5" error, switch to a conformance checking mode for
      ## another language.  (We don't support such mode switchings; it
      ## is nonsense to do anything different from what browsers do.)
      my $doctype_name = $token->{name};
      $doctype_name = '' unless defined $doctype_name;

      if ($doctype_name ne 'html') {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'not HTML5', token => $token);
      } elsif (defined $token->{pubid}) {
        ## Obsolete permitted DOCTYPEs (case-sensitive)
        my $xsysid = {
          '-//W3C//DTD HTML 4.0//EN' => 'http://www.w3.org/TR/REC-html40/strict.dtd',
          '-//W3C//DTD HTML 4.01//EN' => 'http://www.w3.org/TR/html4/strict.dtd',
          '-//W3C//DTD XHTML 1.0 Strict//EN' => 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd',
          '-//W3C//DTD XHTML 1.1//EN' => 'http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd',
        }->{$token->{pubid}};
        if (defined $xsysid and
            (not defined $token->{sysid} or $token->{sysid} eq $xsysid)) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'obs DOCTYPE', token => $token,
                          level => $self->{level}->{obsconforming});
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'not HTML5', token => $token);
        }
      } elsif (defined $token->{sysid}) {
        if ($token->{sysid} eq 'about:legacy-compat') {
           ## <!DOCTYPE HTML SYSTEM "about:legacy-compat">
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'XSLT-compat', token => $token,
                          level => $self->{level}->{should});
        } else {
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'not HTML5', token => $token);
        }
      } else { ## <!DOCTYPE HTML>
        
        #
      }
      
		DATA($self->{'document'}, 'DTD_PUBLIC_ID', $token->{pubid});
		DATA($self->{'document'}, 'DTD_SYSTEM_ID', $token->{sysid});
      
      if ($token->{quirks} or $doctype_name ne 'html') {
        
        DATA($self->{document})->{'manakai_compat_mode'} = 'quirks';
      } elsif (defined $token->{pubid}) {
        my $pubid = $token->{pubid};
        $pubid =~ tr/a-z/A-Z/; ## ASCII case-insensitive.
        my $prefix = [
          "+//SILMARIL//DTD HTML PRO V0R11 19970101//",
          "-//ADVASOFT LTD//DTD HTML 3.0 ASWEDIT + EXTENSIONS//",
          "-//AS//DTD HTML 3.0 ASWEDIT + EXTENSIONS//",
          "-//IETF//DTD HTML 2.0 LEVEL 1//",
          "-//IETF//DTD HTML 2.0 LEVEL 2//",
          "-//IETF//DTD HTML 2.0 STRICT LEVEL 1//",
          "-//IETF//DTD HTML 2.0 STRICT LEVEL 2//",
          "-//IETF//DTD HTML 2.0 STRICT//",
          "-//IETF//DTD HTML 2.0//",
          "-//IETF//DTD HTML 2.1E//",
          "-//IETF//DTD HTML 3.0//",
          "-//IETF//DTD HTML 3.2 FINAL//",
          "-//IETF//DTD HTML 3.2//",
          "-//IETF//DTD HTML 3//",
          "-//IETF//DTD HTML LEVEL 0//",
          "-//IETF//DTD HTML LEVEL 1//",
          "-//IETF//DTD HTML LEVEL 2//",
          "-//IETF//DTD HTML LEVEL 3//",
          "-//IETF//DTD HTML STRICT LEVEL 0//",
          "-//IETF//DTD HTML STRICT LEVEL 1//",
          "-//IETF//DTD HTML STRICT LEVEL 2//",
          "-//IETF//DTD HTML STRICT LEVEL 3//",
          "-//IETF//DTD HTML STRICT//",
          "-//IETF//DTD HTML//",
          "-//METRIUS//DTD METRIUS PRESENTATIONAL//",
          "-//MICROSOFT//DTD INTERNET EXPLORER 2.0 HTML STRICT//",
          "-//MICROSOFT//DTD INTERNET EXPLORER 2.0 HTML//",
          "-//MICROSOFT//DTD INTERNET EXPLORER 2.0 TABLES//",
          "-//MICROSOFT//DTD INTERNET EXPLORER 3.0 HTML STRICT//",
          "-//MICROSOFT//DTD INTERNET EXPLORER 3.0 HTML//",
          "-//MICROSOFT//DTD INTERNET EXPLORER 3.0 TABLES//",
          "-//NETSCAPE COMM. CORP.//DTD HTML//",
          "-//NETSCAPE COMM. CORP.//DTD STRICT HTML//",
          "-//O'REILLY AND ASSOCIATES//DTD HTML 2.0//",
          "-//O'REILLY AND ASSOCIATES//DTD HTML EXTENDED 1.0//",
          "-//O'REILLY AND ASSOCIATES//DTD HTML EXTENDED RELAXED 1.0//",
          "-//SOFTQUAD SOFTWARE//DTD HOTMETAL PRO 6.0::19990601::EXTENSIONS TO HTML 4.0//",
          "-//SOFTQUAD//DTD HOTMETAL PRO 4.0::19971010::EXTENSIONS TO HTML 4.0//",
          "-//SPYGLASS//DTD HTML 2.0 EXTENDED//",
          "-//SQ//DTD HTML 2.0 HOTMETAL + EXTENSIONS//",
          "-//SUN MICROSYSTEMS CORP.//DTD HOTJAVA HTML//",
          "-//SUN MICROSYSTEMS CORP.//DTD HOTJAVA STRICT HTML//",
          "-//W3C//DTD HTML 3 1995-03-24//",
          "-//W3C//DTD HTML 3.2 DRAFT//",
          "-//W3C//DTD HTML 3.2 FINAL//",
          "-//W3C//DTD HTML 3.2//",
          "-//W3C//DTD HTML 3.2S DRAFT//",
          "-//W3C//DTD HTML 4.0 FRAMESET//",
          "-//W3C//DTD HTML 4.0 TRANSITIONAL//",
          "-//W3C//DTD HTML EXPERIMETNAL 19960712//",
          "-//W3C//DTD HTML EXPERIMENTAL 970421//",
          "-//W3C//DTD W3 HTML//",
          "-//W3O//DTD W3 HTML 3.0//",
          "-//WEBTECHS//DTD MOZILLA HTML 2.0//",
          "-//WEBTECHS//DTD MOZILLA HTML//",
        ]; # $prefix
        my $match;
        for (@$prefix) {
          if (substr ($prefix, 0, length $_) eq $_) {
            $match = 1;
            last;
          }
        }
        if ($match or
            $pubid eq "-//W3O//DTD W3 HTML STRICT 3.0//EN//" or
            $pubid eq "-/W3C/DTD HTML 4.0 TRANSITIONAL/EN" or
            $pubid eq "HTML") {
          
          DATA($self->{document})->{'manakai_compat_mode'} = 'quirks';
        } elsif ($pubid =~ m[^-//W3C//DTD HTML 4.01 FRAMESET//] or
                 $pubid =~ m[^-//W3C//DTD HTML 4.01 TRANSITIONAL//]) {
          if (defined $token->{sysid}) {
            
            DATA($self->{document})->{'manakai_compat_mode'} = 'quirks';
          } else {
            
            DATA($self->{document})->{'manakai_compat_mode'} = 'limited quirks';
          }
        } elsif ($pubid =~ m[^-//W3C//DTD XHTML 1.0 FRAMESET//] or
                 $pubid =~ m[^-//W3C//DTD XHTML 1.0 TRANSITIONAL//]) {
          
          DATA($self->{document})->{'manakai_compat_mode'} ='limited quirks';
        } else {
          
        }
      } else {
        
      }
      if (defined $token->{sysid}) {
        my $sysid = $token->{sysid};
        $sysid =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
        if ($sysid eq "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd") {
          ## NOTE: Ensure that |PUBLIC "(limited quirks)" "(quirks)"|
          ## is signaled as in quirks mode!
          DATA($self->{document})->{'manakai_compat_mode'} = 'quirks';
          
        } else {
          
        }
      } else {
        
      }
      
      ## Go to the "before html" insertion mode.
      $token = $self->_get_next_token;
      return;
    } elsif ({
              START_TAG_TOKEN, 1,
              END_TAG_TOKEN, 1,
              END_OF_FILE_TOKEN, 1,
             }->{$token->{type}}) {
      
		unless (DATA($self->{'document'}, 'manakai_is_srcdoc'))
		{
			$self->{parse_error}->(level => $self->{level}->{must}, type => 'no DOCTYPE', token => $token);
			DATA($self->{document})->{'manakai_compat_mode'} = 'quirks';
		}
      ## Go to the "before html" insertion mode.
      ## reprocess
      
      return;
    } elsif ($token->{type} == CHARACTER_TOKEN) {
      if ($token->{data} =~ s/^([\x09\x0A\x0C\x20]+)//) {
        ## Ignore the token

        unless (length $token->{data}) {
          
          ## Stay in the insertion mode.
          $token = $self->_get_next_token;
          redo INITIAL;
        } else {
          
        }
      } else {
        
      }

      $self->{parse_error}->(level => $self->{level}->{must}, type => 'no DOCTYPE', token => $token);
      DATA($self->{document})->{'manakai_compat_mode'} = 'quirks';
      ## Go to the "before html" insertion mode.
      ## reprocess
      return;
    } elsif ($token->{type} == COMMENT_TOKEN) {
      
      my $comment = $self->{document}->createComment ($token->{data});
      $self->{document}->appendChild ($comment);
      
      ## Stay in the insertion mode.
      $token = $self->_get_next_token;
      redo INITIAL;
    } else {
      die "$0: $token->{type}: Unknown token type";
    }
  } # INITIAL

  die "$0: _tree_construction_initial: This should be never reached";
} # _tree_construction_initial

sub _tree_construction_root_element ($) {
  my $self = shift;

  ## NOTE: The "before html" insertion mode.
  
  B: {
      if ($token->{type} == DOCTYPE_TOKEN) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'in html:#DOCTYPE', token => $token);
        ## Ignore the token
        $token = $self->_get_next_token;
        redo B;
      } elsif ($token->{type} == COMMENT_TOKEN) {
        
        my $comment = $self->{document}->createComment ($token->{data});
        $self->{document}->appendChild ($comment);
        ## Stay in the insertion mode.
        $token = $self->_get_next_token;
        redo B;
      } elsif ($token->{type} == CHARACTER_TOKEN) {
        if ($token->{data} =~ s/^([\x09\x0A\x0C\x20]+)//) {
          ## Ignore the token.

          unless (length $token->{data}) {
            
            ## Stay in the insertion mode.
            $token = $self->_get_next_token;
            redo B;
          } else {
            
          }
        } else {
          
        }

        $self->{application_cache_selection}->(undef);

        #
      } elsif ($token->{type} == START_TAG_TOKEN) {
        if ($token->{tag_name} eq 'html') {
          my $root_element;
          
      $root_element = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{ $token->{attributes}}) {
          my $attr_t =  $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $root_element->setAttributeNodeNS($attr);
        }
      
        DATA($root_element, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($root_element, manakai_source_column => $token->{column})
            if defined $token->{column};
      
          $self->{document}->setDocumentElement($root_element);
          push @{$self->{open_elements}},
              [$root_element, $el_category->{html}];

          if ($token->{attributes}->{manifest}) {
            
            ## XXX resolve URL and drop fragment
            ## <http://html5.org/tools/web-apps-tracker?from=3479&to=3480>
            ## <http://manakai.g.hatena.ne.jp/task/2/95>
            $self->{application_cache_selection}
                ->($token->{attributes}->{manifest}->{value});
          } else {
            
            $self->{application_cache_selection}->(undef);
          }

          

          $token = $self->_get_next_token;
          return; ## Go to the "before head" insertion mode.
        } else {
          
          #
        }
      } elsif ($token->{type} == END_TAG_TOKEN) {
       if ({
         head => 1, body => 1, html => 1, br => 1,
       }->{$token->{tag_name}}) {
         
         #
       } else {
         
         $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                         text => $token->{tag_name},
                         token => $token);
         ## Ignore the token.
         $token = $self->_get_next_token;
         redo B;
       }
     } elsif ($token->{type} == END_OF_FILE_TOKEN) {
       
       #
     } else {
        die "$0: $token->{type}: Unknown token type";
      }

    my $root_element;
    
      $root_element = $self->{document}->createElementNS((HTML_NS), 'html');
    
        DATA($root_element, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($root_element, manakai_source_column => $token->{column})
            if defined $token->{column};
      
    $self->{document}->setDocumentElement($root_element);
    push @{$self->{open_elements}}, [$root_element, $el_category->{html}];

    $self->{application_cache_selection}->(undef);

    ## NOTE: Reprocess the token.
    
    return; ## Go to the "before head" insertion mode.
  } # B

  die "$0: _tree_construction_root_element: This should never be reached";
} # _tree_construction_root_element

sub _reset_insertion_mode ($) {
  my $self = shift;

    ## Step 1
    my $last;
    
    ## Step 2
    my $foreign;
    
    ## Step 3
    my $i = -1;
    my $node = $self->{open_elements}->[$i];
    
    ## LOOP: Step 4
    LOOP: {
      if ($self->{open_elements}->[0]->[0] eq $node->[0]) {
        $last = 1;
        if (defined $self->{inner_html_node}) {
          
          $node = $self->{inner_html_node};
        } else {
          die "_reset_insertion_mode: t27";
        }
      }
      
      ## Step 5..15
      my $new_mode;
      if ($node->[1] & FOREIGN_EL) {
        
        ## NOTE: Strictly spaking, this case should only apply to MathML and
        ## SVG elements.  Currently the HTML syntax supports only MathML and
        ## SVG elements as foreigners.
        $foreign = 1;
        #$new_mode = IN_BODY_IM | IN_FOREIGN_CONTENT_IM;
      } elsif ($node->[1] == TABLE_CELL_EL) {
        if ($last) {
          
          #
        } else {
          
          $new_mode = IN_CELL_IM;
        }
      } else {
        
        $new_mode = {
                      select => IN_SELECT_IM,
                      ## NOTE: |option| and |optgroup| do not set
                      ## insertion mode to "in select" by themselves.
                      tr => IN_ROW_IM,
                      tbody => IN_TABLE_BODY_IM,
                      thead => IN_TABLE_BODY_IM,
                      tfoot => IN_TABLE_BODY_IM,
                      caption => IN_CAPTION_IM,
                      colgroup => IN_COLUMN_GROUP_IM,
                      table => IN_TABLE_IM,
                      head => IN_BODY_IM, # not in head!
                      body => IN_BODY_IM,
                      frameset => IN_FRAMESET_IM,
                     }->{$node->[0]->tagName};
      }
      $self->{insertion_mode} = $new_mode and last LOOP if defined $new_mode;
      
      ## Step 16
      if ($node->[1] == HTML_EL) {
        ## NOTE: Commented out in the spec (HTML5 revision 3894).
        #unless (defined $self->{head_element}) {
          
          $self->{insertion_mode} = BEFORE_HEAD_IM;
        #} else {
          ## ISSUE: Can this state be reached?
          
        #  $self->{insertion_mode} = AFTER_HEAD_IM;
        #}
        last LOOP;
      } else {
        
      }
      
      ## Step 17
		if ($last)
		{
			$self->{insertion_mode} = IN_BODY_IM;
			last LOOP;
		}
      
      ## Step 18
      $i--;
      $node = $self->{open_elements}->[$i];
      
      ## Step 19
      redo LOOP;
    } # LOOP

  ## END: Step 20
  if ($foreign) {
    $self->{insertion_mode} |= IN_FOREIGN_CONTENT_IM;
  }
} # _reset_insertion_mode

  my $parse_rcdata = sub ($$$$) {
    my ($self, $insert, $open_tables, $parse_refs) = @_;

    ## Step 1
    my $start_tag_name = $token->{tag_name};
    
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  

    ## Step 2
    if ($parse_refs) {
      $self->{state} = RCDATA_STATE;
    } else {
      $self->{state} = RAWTEXT_STATE;
    }
    delete $self->{escape}; # MUST

    ## Step 3, 4
    $self->{insertion_mode} |= IN_CDATA_RCDATA_IM;

    
    $token = $self->_get_next_token;
  }; # $parse_rcdata

  my $script_start_tag = sub ($$$) {
    my ($self, $insert, $open_tables) = @_;
    
    ## Step 1
    my $script_el;
    
      $script_el = $self->{document}->createElementNS((HTML_NS), 'script');
    
        for my $attr_name (keys %{ $token->{attributes}}) {
          my $attr_t =  $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $script_el->setAttributeNodeNS($attr);
        }
      
        DATA($script_el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($script_el, manakai_source_column => $token->{column})
            if defined $token->{column};
      

    ## Step 2
    ## TODO: mark as "parser-inserted"

    ## Step 3
    ## TODO: Mark as "already executed", if ...

    ## Step 4 (HTML5 revision 2702)
    $insert->($self, $script_el, $open_tables);
    push @{$self->{open_elements}}, [$script_el, $el_category->{script}];

    ## Step 5
    $self->{state} = SCRIPT_DATA_STATE;
    delete $self->{escape}; # MUST

    ## Step 6-7
    $self->{insertion_mode} |= IN_CDATA_RCDATA_IM;

    
    $token = $self->_get_next_token;
  }; # $script_start_tag

  my $formatting_end_tag = sub {
    my ($self, $active_formatting_elements, $open_tables, $end_tag_token) = @_;
    my $tag_name = $end_tag_token->{tag_name};

    ## NOTE: The adoption agency algorithm (AAA).

    FET: {
      ## Step 1
      my $formatting_element;
      my $formatting_element_i_in_active;
      AFE: for (reverse 0..$#$active_formatting_elements) {
        if ($active_formatting_elements->[$_]->[0] eq '#marker') {
          
          last AFE;
        } elsif ($active_formatting_elements->[$_]->[0]->tagName
                     eq $tag_name) {
          
          $formatting_element = $active_formatting_elements->[$_];
          $formatting_element_i_in_active = $_;
          last AFE;
        }
      } # AFE
      unless (defined $formatting_element) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag', text => $tag_name, token => $end_tag_token);
        ## Ignore the token
        $token = $self->_get_next_token;
        return;
      }
      ## has an element in scope
      my $in_scope = 1;
      my $formatting_element_i_in_open;  
      INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
        my $node = $self->{open_elements}->[$_];
        if ($node->[0] eq $formatting_element->[0]) {
          if ($in_scope) {
            
            $formatting_element_i_in_open = $_;
            last INSCOPE;
          } else { # in open elements but not in scope
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => $token->{tag_name},
                            token => $end_tag_token);
            ## Ignore the token
            $token = $self->_get_next_token;
            return;
          }
        } elsif ($node->[1] & SCOPING_EL) {
          
          $in_scope = 0;
        }
      } # INSCOPE
      unless (defined $formatting_element_i_in_open) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                        text => $token->{tag_name},
                        token => $end_tag_token);
        pop @$active_formatting_elements; # $formatting_element
        $token = $self->_get_next_token; ## TODO: ok?
        return;
      }
      if (not $self->{open_elements}->[-1]->[0] eq $formatting_element->[0]) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                        text => $self->{open_elements}->[-1]->[0]
                            ->tagName,
                        token => $end_tag_token);
      }
      
      ## Step 2
      my $furthest_block;
      my $furthest_block_i_in_open;
      OE: for (reverse 0..$#{$self->{open_elements}}) {
        my $node = $self->{open_elements}->[$_];
        if (not ($node->[1] & FORMATTING_EL) and 
            #not $phrasing_category->{$node->[1]} and
            ($node->[1] & SPECIAL_EL or
             $node->[1] & SCOPING_EL)) { ## Scoping is redundant, maybe
          
          $furthest_block = $node;
          $furthest_block_i_in_open = $_;
	  ## NOTE: The topmost (eldest) node.
        } elsif ($node->[0] eq $formatting_element->[0]) {
          
          last OE;
        }
      } # OE
      
      ## Step 3
      unless (defined $furthest_block) { # MUST
        
        splice @{$self->{open_elements}}, $formatting_element_i_in_open;
        splice @$active_formatting_elements, $formatting_element_i_in_active, 1;
        $token = $self->_get_next_token;
        return;
      }
      
      ## Step 4
      my $common_ancestor_node = $self->{open_elements}->[$formatting_element_i_in_open - 1];
      
      ## Step 5
      my $bookmark_prev_el
        = $active_formatting_elements->[$formatting_element_i_in_active - 1]
          ->[0];
      
      ## Step 6
      my $node = $furthest_block;
      my $node_i_in_open = $furthest_block_i_in_open;
      my $last_node = $furthest_block;
      S7: {
        ## Step 6.1
        $node_i_in_open--;
        $node = $self->{open_elements}->[$node_i_in_open];
        
        ## Step 6.2
        my $node_i_in_active;
        my $node_token;
        S7S2: {
          for (reverse 0..$#$active_formatting_elements) {
            if ($active_formatting_elements->[$_]->[0] eq $node->[0]) {
              
              $node_i_in_active = $_;
              $node_token = $active_formatting_elements->[$_]->[2];
              last S7S2;
            }
          }
          splice @{$self->{open_elements}}, $node_i_in_open, 1;
          redo S7;
        } # S7S2
        
        ## Step 6.3
        last S7 if $node->[0] eq $formatting_element->[0];
        
        ## Step 6.4
        if ($last_node->[0] eq $furthest_block->[0]) {
          
          $bookmark_prev_el = $node->[0];
        }
        
        ## Step 6.5
        if ($node->[0]->hasChildNodes ()) {
          
          my $new_element = [];
          
      $new_element->[0] = $self->{document}->createElementNS((HTML_NS), $node_token->{tag_name});
    
        for my $attr_name (keys %{ $node_token->{attributes}}) {
          my $attr_t =  $node_token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $new_element->[0]->setAttributeNodeNS($attr);
        }
      
        DATA($new_element->[0], manakai_source_line => $node_token->{line})
            if defined $node_token->{line};
        DATA($new_element->[0], manakai_source_column => $node_token->{column})
            if defined $node_token->{column};
      
          $new_element->[1] = $node->[1];
          $new_element->[2] = $node_token;
          $active_formatting_elements->[$node_i_in_active] = $new_element;
          $self->{open_elements}->[$node_i_in_open] = $new_element;
          $node = $new_element;
        }
        
        ## Step 6.6
        $node->[0]->appendChild ($last_node->[0]);
        
        ## Step 6.7
        $last_node = $node;
        
        ## Step 6.8
        redo S7;
      } # S7  
      
      ## Step 7
      if ($common_ancestor_node->[1] & TABLE_ROWS_EL) {
        ## Foster parenting.
        my $foster_parent_element;
        my $next_sibling;
        OE: for (reverse 0..$#{$self->{open_elements}}) {
          if ($self->{open_elements}->[$_]->[1] == TABLE_EL) {
            
            $foster_parent_element = $self->{open_elements}->[$_ - 1]->[0];
            $next_sibling = $self->{open_elements}->[$_]->[0];
            undef $next_sibling
                unless $next_sibling->parentNode eq $foster_parent_element;
            last OE;
          }
        } # OE
        $foster_parent_element ||= $self->{open_elements}->[0]->[0];

        $foster_parent_element->insertBefore ($last_node->[0], $next_sibling);
        $open_tables->[-1]->[1] = 1; # tainted
      } else {
        
        $common_ancestor_node->[0]->appendChild ($last_node->[0]);
      }
      
      ## Step 8
          my $new_element = [];
          
      $new_element->[0] = $self->{document}->createElementNS((HTML_NS), $formatting_element->[2]->{tag_name});
    
        for my $attr_name (keys %{ $formatting_element->[2]->{attributes}}) {
          my $attr_t =  $formatting_element->[2]->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $new_element->[0]->setAttributeNodeNS($attr);
        }
      
        DATA($new_element->[0], manakai_source_line => $formatting_element->[2]->{line})
            if defined $formatting_element->[2]->{line};
        DATA($new_element->[0], manakai_source_column => $formatting_element->[2]->{column})
            if defined $formatting_element->[2]->{column};
      
          $new_element->[1] = $formatting_element->[1];
          $new_element->[2] = $formatting_element->[2];
      
      ## Step 9
      my @cn = $furthest_block->[0]->childNodes;
      $new_element->[0]->appendChild($_) for @cn;
      
      ## Step 10
      $furthest_block->[0]->appendChild ($new_element->[0]);
      
      ## Step 11
      my $i;
      AFE: for (reverse 0..$#$active_formatting_elements) {
        if ($active_formatting_elements->[$_]->[0] eq $formatting_element->[0]) {
          
          splice @$active_formatting_elements, $_, 1;
          $i-- and last AFE if defined $i;
        } elsif ($active_formatting_elements->[$_]->[0] eq $bookmark_prev_el) {
          
          $i = $_;
        }
      } # AFE
      splice @$active_formatting_elements, $i + 1, 0, $new_element;
      
      ## Step 12
      undef $i;
      OE: for (reverse 0..$#{$self->{open_elements}}) {
        if ($self->{open_elements}->[$_]->[0] eq $formatting_element->[0]) {
          
          splice @{$self->{open_elements}}, $_, 1;
          $i-- and last OE if defined $i;
        } elsif ($self->{open_elements}->[$_]->[0] eq $furthest_block->[0]) {
          
          $i = $_;
        }
      } # OE
      splice @{$self->{open_elements}}, $i + 1, 0, $new_element;
      
      ## Step 13
      redo FET;
    } # FET
  }; # $formatting_end_tag

  my $reconstruct_active_formatting_elements = sub ($$$$) { # MUST
    my ($self, $insert, $active_formatting_elements, $open_tables) = @_;

    ## Step 1
    return unless @$active_formatting_elements;

    ## Step 3
    my $i = -1;
    my $entry = $active_formatting_elements->[$i];

    ## Step 2
    return if $entry->[0] eq '#marker';
    for (@{$self->{open_elements}}) {
      if ($entry->[0] eq $_->[0]) {
        
        return;
      }
    }
    
    S4: {
      ## Step 4
      last S4 if $active_formatting_elements->[0]->[0] eq $entry->[0];

      ## Step 5
      $i--;
      $entry = $active_formatting_elements->[$i];

      ## Step 6
      if ($entry->[0] eq '#marker') {
        
        #
      } else {
        my $in_open_elements;
        OE: for (@{$self->{open_elements}}) {
          if ($entry->[0] eq $_->[0]) {
            
            $in_open_elements = 1;
            last OE;
          }
        }
        if ($in_open_elements) {
          
          #
        } else {
          ## NOTE: <!DOCTYPE HTML><p><b><i><u></p> <p>X
          
          redo S4;
        }
      }

      ## Step 7
      $i++;
      $entry = $active_formatting_elements->[$i];
    } # S4

    S7: {
      ## Step 8
      my $clone = [$entry->[0]->cloneNode (0), $entry->[1]];
    
      ## Step 9
      $insert->($self, $clone->[0], $open_tables);
      push @{$self->{open_elements}}, $clone;
      
      ## Step 10
      $active_formatting_elements->[$i] = $self->{open_elements}->[-1];

      ## Step 11
      unless ($clone->[0] eq $active_formatting_elements->[-1]->[0]) {
        
        ## Step 7'
        $i++;
        $entry = $active_formatting_elements->[$i];
        
        redo S7;
      }

      
    } # S7
  }; # $reconstruct_active_formatting_elements

  my $clear_up_to_marker = sub ($) {
	 my $active_formatting_elements = $_[0];
    for (reverse 0..$#$active_formatting_elements) {
      if ($active_formatting_elements->[$_]->[0] eq '#marker') {
        
        splice @$active_formatting_elements, $_;
        return;
      }
    }

    
  }; # $clear_up_to_marker

  my $insert_to_current = sub {
    #my ($self, $child, $open_tables) = @_;
    $_[0]->{open_elements}->[-1]->[0]->appendChild ($_[1]);
 }; # insert_to_current

  ## Foster parenting.  Note that there are three "foster parenting"
  ## code in the parser: for elements (this one), for texts, and for
  ## elements in the AAA code.
  my $insert_to_foster = sub {
    my ($self, $child, $open_tables) = @_;
    if ($self->{open_elements}->[-1]->[1] & TABLE_ROWS_EL) {
      # MUST
      my $foster_parent_element;
      my $next_sibling;
      OE: for (reverse 0..$#{$self->{open_elements}}) {
        if ($self->{open_elements}->[$_]->[1] == TABLE_EL) {
          
          $foster_parent_element = $self->{open_elements}->[$_ - 1]->[0];
          $next_sibling = $self->{open_elements}->[$_]->[0];
          undef $next_sibling
              unless $next_sibling->parentNode eq $foster_parent_element;
          last OE;
        }
      } # OE
      $foster_parent_element ||= $self->{open_elements}->[0]->[0];

		# This conditional bit is by TOBY
		if ($next_sibling)
		{
			$foster_parent_element->insertBefore ($child, $next_sibling);
		}
		else
		{
			$foster_parent_element->appendChild($child);
		}
      $open_tables->[-1]->[1] = 1; # tainted
    } else {
      
      $self->{open_elements}->[-1]->[0]->appendChild ($child);
    }
  }; # $insert_to_foster

sub _tree_construction_main ($) {
  my $self = shift;

  ## "List of active formatting elements".  Each item in this array is
  ## an array reference, which contains: [0] - the element node; [1] -
  ## the local name of the element; [2] - the token that is used to
  ## create [0].
  my $active_formatting_elements = [];

  my $insert;

  ## NOTE: $open_tables->[-1]->[0] is the "current table" element node.
  ## NOTE: $open_tables->[-1]->[1] is the "tainted" flag (OBSOLETE; unused).
  ## NOTE: $open_tables->[-1]->[2] is set false when non-Text node inserted.
  my $open_tables = [[$self->{open_elements}->[0]->[0]]];

  $insert = $insert_to_current;

  ## NOTE: Insert a character (MUST): When a character is inserted, if
  ## the last node that was inserted by the parser is a Text node and
  ## the character has to be inserted after that node, then the
  ## character is appended to the Text node.  However, if any other
  ## node is inserted by the parser, then a new Text node is created
  ## and the character is appended as that Text node.  If I'm not
  ## wrong, for a parser with scripting disabled, there are only two
  ## cases where this occurs.  One is the case where an element node
  ## is inserted to the |head| element.  This is covered by using the
  ## |$self->{head_element_inserted}| flag.  Another is the case where
  ## an element or comment is inserted into the |table| subtree while
  ## foster parenting happens.  This is covered by using the [2] flag
  ## of the |$open_tables| structure.  All other cases are handled
  ## simply by calling |appendText| method.

  ## TODO: |<body><script>document.write("a<br>");
  ## document.body.removeChild (document.body.lastChild);
  ## document.write ("b")</script>|

  B: while (1) {

    ## The "in table text" insertion mode.
    if ($self->{insertion_mode} & TABLE_IMS and
        not $self->{insertion_mode} & IN_FOREIGN_CONTENT_IM and
        not $self->{insertion_mode} & IN_CDATA_RCDATA_IM) {
      C: {
        my $s;
        if ($token->{type} == CHARACTER_TOKEN) {
          
          $self->{pending_chars} ||= [];
          push @{$self->{pending_chars}}, $token;
          $token = $self->_get_next_token;
          next B;
        } else {
          if ($self->{pending_chars}) {
            $s = join '', map { $_->{data} } @{$self->{pending_chars}};
            delete $self->{pending_chars};
            if ($s =~ /[^\x09\x0A\x0C\x0D\x20]/) {
              
              #
            } else {
              
              #$self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $s);
              $self->{open_elements}->[-1]->[0]->appendChild
                  ($self->{document}->createTextNode ($s));
              last C;
            }
          } else {
            
            last C;
          }
        }

        ## Foster parenting.
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'in table:#text', token => $token);

        ## NOTE: As if in body, but insert into the foster parent element.
        $reconstruct_active_formatting_elements
			->($self, $insert_to_foster, $active_formatting_elements, $open_tables);
            
        if ($self->{open_elements}->[-1]->[1] & TABLE_ROWS_EL) {
          # MUST
          my $foster_parent_element;
          my $next_sibling;
          OE: for (reverse 0..$#{$self->{open_elements}}) {
            if ($self->{open_elements}->[$_]->[1] == TABLE_EL) {
              
              $foster_parent_element = $self->{open_elements}->[$_ - 1]->[0];
              $next_sibling = $self->{open_elements}->[$_]->[0];
              undef $next_sibling
                unless $next_sibling->parentNode eq $foster_parent_element;
              last OE;
            }
          } # OE
          $foster_parent_element ||= $self->{open_elements}->[0]->[0];

          
          $foster_parent_element->insertBefore
              ($self->{document}->createTextNode ($s), $next_sibling);

          $open_tables->[-1]->[1] = 1; # tainted
          $open_tables->[-1]->[2] = 1; # ~node inserted
        } else {
          ## NOTE: Fragment case or in a foster parent'ed element
          ## (e.g. |<table><span>a|).  In fragment case, whether the
          ## character is appended to existing node or a new node is
          ## created is irrelevant, since the foster parent'ed nodes
          ## are discarded and fragment parsing does not invoke any
          ## script.
          
          $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $s);
        }
      } # C
    } # TABLE_IMS

    if ($token->{type} == DOCTYPE_TOKEN) {
      
      $self->{parse_error}->(level => $self->{level}->{must}, type => 'in html:#DOCTYPE', token => $token);
      ## Ignore the token
      ## Stay in the phase
      $token = $self->_get_next_token;
      next B;
    } elsif ($token->{type} == START_TAG_TOKEN and
             $token->{tag_name} eq 'html') {
      if ($self->{insertion_mode} == AFTER_HTML_BODY_IM) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'after html', text => 'html', token => $token);
        $self->{insertion_mode} = AFTER_BODY_IM;
      } elsif ($self->{insertion_mode} == AFTER_HTML_FRAMESET_IM) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'after html', text => 'html', token => $token);
        $self->{insertion_mode} = AFTER_FRAMESET_IM;
      } else {
        
      }

      
      $self->{parse_error}->(level => $self->{level}->{must}, type => 'not first start tag', token => $token);
      my $top_el = $self->{open_elements}->[0]->[0];
      for my $attr_name (keys %{$token->{attributes}}) {
        eval {
        unless ($top_el->hasAttributeNS (undef, $attr_name)) {
          
          $top_el->setAttributeNS
            (undef, $attr_name, $token->{attributes}->{$attr_name}->{value});
        }
	};
      }
      
      $token = $self->_get_next_token;
      next B;
    } elsif ($token->{type} == COMMENT_TOKEN) {
      my $comment = $self->{document}->createComment ($token->{data});
      if ($self->{insertion_mode} & AFTER_HTML_IMS) {
        
        $self->{document}->appendChild ($comment);
      } elsif ($self->{insertion_mode} == AFTER_BODY_IM) {
        
        $self->{open_elements}->[0]->[0]->appendChild ($comment);
      } else {
        
        $self->{open_elements}->[-1]->[0]->appendChild ($comment);
        $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted
      }
      $token = $self->_get_next_token;
      next B;
    } elsif ($self->{insertion_mode} & IN_CDATA_RCDATA_IM) {
      if ($token->{type} == CHARACTER_TOKEN) {
        $token->{data} =~ s/^\x0A// if $self->{ignore_newline};
        delete $self->{ignore_newline};

        if (length $token->{data}) {
          
          $self->{open_elements}->[-1]->[0]->appendText #TODO - check
              ($token->{data});
        } else {
          
        }
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{type} == END_TAG_TOKEN) {
        delete $self->{ignore_newline};

        if ($token->{tag_name} eq 'script') {
          
          
          ## Para 1-2
          my $script = pop @{$self->{open_elements}};
          
          ## Para 3
          $self->{insertion_mode} &= ~ IN_CDATA_RCDATA_IM;

          ## Para 4
          ## TODO: $old_insertion_point = $current_insertion_point;
          ## TODO: $current_insertion_point = just before $self->{nc};

          ## Para 5
          ## TODO: Run the $script->[0].

          ## Para 6
          ## TODO: $current_insertion_point = $old_insertion_point;

          ## Para 7
          ## TODO: if ($pending_external_script) {
            ## TODO: ...
          ## TODO: }

          $token = $self->_get_next_token;
          next B;
        } else {
          
 
          pop @{$self->{open_elements}};

          $self->{insertion_mode} &= ~ IN_CDATA_RCDATA_IM;
          $token = $self->_get_next_token;
          next B;
        }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        delete $self->{ignore_newline};

        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                        text => $self->{open_elements}->[-1]->[0]
                            ->tagName,
                        token => $token);

        #if ($self->{open_elements}->[-1]->[1] == SCRIPT_EL) {
        #  ## TODO: Mark as "already executed"
        #}

        pop @{$self->{open_elements}};

        $self->{insertion_mode} &= ~ IN_CDATA_RCDATA_IM;
        ## Reprocess.
        next B;
      } else {
        die "$0: $token->{type}: In CDATA/RCDATA: Unknown token type";        
      }
    } elsif ($self->{insertion_mode} & IN_FOREIGN_CONTENT_IM) {
      if ($token->{type} == CHARACTER_TOKEN) {
        

        $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $token->{data});

        if ($token->{data} =~ /[^\x09\x0A\x0C\x0D\x20]/) {
          delete $self->{frameset_ok};
        }

        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{type} == START_TAG_TOKEN) {
        if ((not {mglyph => 1, malignmark => 1}->{$token->{tag_name}} and
             $self->{open_elements}->[-1]->[1] & FOREIGN_FLOW_CONTENT_EL) or
            not ($self->{open_elements}->[-1]->[1] & FOREIGN_EL) or
            ($token->{tag_name} eq 'svg' and
             $self->{open_elements}->[-1]->[1] == MML_AXML_EL)) {
          ## NOTE: "using the rules for secondary insertion mode"then"continue"
          
          #
        } elsif ({
                  b => 1, big => 1, blockquote => 1, body => 1, br => 1,
                  center => 1, code => 1, dd => 1, div => 1, dl => 1, dt => 1,
                  em => 1, embed => 1, h1 => 1, h2 => 1, h3 => 1,
                  h4 => 1, h5 => 1, h6 => 1, head => 1, hr => 1, i => 1,
                  img => 1, li => 1, listing => 1, menu => 1, meta => 1,
                  nobr => 1, ol => 1, p => 1, pre => 1, ruby => 1, s => 1,
                  small => 1, span => 1, strong => 1, strike => 1, sub => 1,
                  sup => 1, table => 1, tt => 1, u => 1, ul => 1, var => 1,
                 }->{$token->{tag_name}} or
                 ($token->{tag_name} eq 'font' and
                  ($token->{attributes}->{color} or
                   $token->{attributes}->{face} or
                   $token->{attributes}->{size}))) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                          text => $self->{open_elements}->[-1]->[0]
                              ->tagName,
                          token => $token);

          pop @{$self->{open_elements}}
              while $self->{open_elements}->[-1]->[1] & FOREIGN_EL;

          $self->{insertion_mode} &= ~ IN_FOREIGN_CONTENT_IM;
          ## Reprocess.
          next B;
        } else {
          my $nsuri = $self->{open_elements}->[-1]->[0]->namespaceURI;
          my $tag_name = $token->{tag_name};
          if ($nsuri eq (SVG_NS)) {
            $tag_name = {
               altglyph => 'altGlyph',
               altglyphdef => 'altGlyphDef',
               altglyphitem => 'altGlyphItem',
               animatecolor => 'animateColor',
               animatemotion => 'animateMotion',
               animatetransform => 'animateTransform',
               clippath => 'clipPath',
               feblend => 'feBlend',
               fecolormatrix => 'feColorMatrix',
               fecomponenttransfer => 'feComponentTransfer',
               fecomposite => 'feComposite',
               feconvolvematrix => 'feConvolveMatrix',
               fediffuselighting => 'feDiffuseLighting',
               fedisplacementmap => 'feDisplacementMap',
               fedistantlight => 'feDistantLight',
               feflood => 'feFlood',
               fefunca => 'feFuncA',
               fefuncb => 'feFuncB',
               fefuncg => 'feFuncG',
               fefuncr => 'feFuncR',
               fegaussianblur => 'feGaussianBlur',
               feimage => 'feImage',
               femerge => 'feMerge',
               femergenode => 'feMergeNode',
               femorphology => 'feMorphology',
               feoffset => 'feOffset',
               fepointlight => 'fePointLight',
               fespecularlighting => 'feSpecularLighting',
               fespotlight => 'feSpotLight',
               fetile => 'feTile',
               feturbulence => 'feTurbulence',
               foreignobject => 'foreignObject',
               glyphref => 'glyphRef',
               lineargradient => 'linearGradient',
               radialgradient => 'radialGradient',
               #solidcolor => 'solidColor', ## NOTE: Commented in spec (SVG1.2)
               textpath => 'textPath',  
            }->{$tag_name} || $tag_name;
          }

          ## "adjust SVG attributes" (SVG only) - done in insert-element-f

          ## "adjust foreign attributes" - done in insert-element-f

          
    {
      my $el;
      
      $el = $self->{document}->createElementNS($nsuri, $tag_name);
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr;
			 if (defined $foreign_attr_xname->{ $attr_name })
			 {
				 my $xmlnsuri = $foreign_attr_xname->{ $attr_name }->[0];
				 my $qname = join ':', @{$foreign_attr_xname->{ $attr_name }->[1]};
				 $qname =~ s/(^:)|(:$)//;
				 $attr = $self->{document}->createAttributeNS($xmlnsuri, $qname);
			 }
			 elsif ($nsuri eq (MML_NS) && $attr_name eq 'definitionurl')
			 {
				 $attr = $self->{document}->createAttributeNS((MML_NS), 'definitionURL');
			 }
			 elsif ($nsuri eq (MML_NS) )
			 {
				 $attr = $self->{document}->createAttributeNS((MML_NS), $attr_name);
			 }
			 elsif ($nsuri eq (SVG_NS) )
			 {
				 $attr = $self->{document}->createAttributeNS(
					(SVG_NS), ($svg_attr_name->{$attr_name} || $attr_name));
			 }
			 unless ($attr)
			 {
				 $attr = $self->{document}->createAttributeNS($nsuri, $attr_name);
			 }
			 unless ($attr)
			 {
				 $attr = $self->{document}->createAttribute($attr_name);
			 }
			 if ($attr)
			 {
				 $attr->setValue ($attr_t->{value});
				 DATA($attr, manakai_source_line => $attr_t->{line});
				 DATA($attr, manakai_source_column => $attr_t->{column});
				 $el->setAttributeNodeNS ($attr);
			 }
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, ($el_category_f->{$nsuri}->{ $tag_name} || 0) | FOREIGN_EL];

      if ( $token->{attributes}->{xmlns} and  $token->{attributes}->{xmlns}->{value} ne ($nsuri)) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad namespace', token =>  $token);
## TODO: Error type documentation
      }
      if ( $token->{attributes}->{'xmlns:xlink'} and
           $token->{attributes}->{'xmlns:xlink'}->{value} ne q<http://www.w3.org/1999/xlink>) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad namespace', token =>  $token);
      }
    }
  

          if ($self->{self_closing}) {
            pop @{$self->{open_elements}};
            delete $self->{self_closing};
          } else {
            
          }

          $token = $self->_get_next_token;
          next B;
        }
      } elsif ($token->{type} == END_TAG_TOKEN) {
        ## NOTE: "using the rules for secondary insertion mode" then "continue"
        if ($token->{tag_name} eq 'script') {
          
          #
          ## XXXscript: Execute script here.
        } else {
          
          #
        }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                        text => $self->{open_elements}->[-1]->[0]
                            ->tagName,
                        token => $token);

        pop @{$self->{open_elements}}
            while $self->{open_elements}->[-1]->[1] & FOREIGN_EL;

        ## NOTE: |<span><svg>| ... two parse errors, |<svg>| ... a parse error.

        $self->{insertion_mode} &= ~ IN_FOREIGN_CONTENT_IM;
        ## Reprocess.
        next B;
      } else {
        die "$0: $token->{type}: Unknown token type";        
      }
    }

    if ($self->{insertion_mode} & HEAD_IMS) {
      if ($token->{type} == CHARACTER_TOKEN) {
        if ($token->{data} =~ s/^([\x09\x0A\x0C\x20]+)//) {
          unless ($self->{insertion_mode} == BEFORE_HEAD_IM) {
            if ($self->{head_element_inserted}) {
              
              $self->{open_elements}->[-1]->[0]->appendChild
                ($self->{document}->createTextNode ($1));
              delete $self->{head_element_inserted};
              ## NOTE: |</head> <link> |
              #
            } else {
              
              $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $1);
              ## NOTE: |</head> &#x20;|
              #
            }
          } else {
            
            ## Ignore the token.
            #
          }
          unless (length $token->{data}) {
            
            $token = $self->_get_next_token;
            next B;
          }
## TODO: set $token->{column} appropriately
        }

        if ($self->{insertion_mode} == BEFORE_HEAD_IM) {
          
          ## As if <head>
          
      $self->{head_element} = $self->{document}->createElementNS((HTML_NS), 'head');
    
        DATA($self->{head_element}, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($self->{head_element}, manakai_source_column => $token->{column})
            if defined $token->{column};
      
          $self->{open_elements}->[-1]->[0]->appendChild ($self->{head_element});
          push @{$self->{open_elements}},
              [$self->{head_element}, $el_category->{head}];

          ## Reprocess in the "in head" insertion mode...
          pop @{$self->{open_elements}};

          ## Reprocess in the "after head" insertion mode...
        } elsif ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
          
          ## As if </noscript>
          pop @{$self->{open_elements}};
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript:#text', token => $token);
          
          ## Reprocess in the "in head" insertion mode...
          ## As if </head>
          pop @{$self->{open_elements}};

          ## Reprocess in the "after head" insertion mode...
        } elsif ($self->{insertion_mode} == IN_HEAD_IM) {
          
          pop @{$self->{open_elements}};

          ## Reprocess in the "after head" insertion mode...
        } else {
          
        }

        ## "after head" insertion mode
        ## As if <body>
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), 'body');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
       DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{'body'} || 0];
    }
  
        $self->{insertion_mode} = IN_BODY_IM;
        ## The "frameset-ok" flag is left unchanged in this case.
        ## Reporcess the token.
        next B;
      } elsif ($token->{type} == START_TAG_TOKEN) {
        if ($token->{tag_name} eq 'head') {
          if ($self->{insertion_mode} == BEFORE_HEAD_IM) {
            
            
      $self->{head_element} = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{ $token->{attributes}}) {
          my $attr_t =  $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr,manakai_source_line => $attr_t->{line});
          DATA($attr,manakai_source_column => $attr_t->{column});
          $self->{head_element}->setAttributeNodeNS ($attr);
        }
      
        DATA($self->{head_element}, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($self->{head_element}, manakai_source_column => $token->{column})
            if defined $token->{column};
      
            $self->{open_elements}->[-1]->[0]->appendChild($self->{head_element});
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];
            $self->{insertion_mode} = IN_HEAD_IM;
            
            $token = $self->_get_next_token;
            next B;
          } elsif ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after head', text => 'head',
                            token => $token);
            ## Ignore the token
            
            $token = $self->_get_next_token;
            next B;
          } else {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in head:head',
                            token => $token); # or in head noscript
            ## Ignore the token
            
            $token = $self->_get_next_token;
            next B;
          }
        } elsif ($self->{insertion_mode} == BEFORE_HEAD_IM) {
          
          ## As if <head>
          
      $self->{head_element} = $self->{document}->createElementNS((HTML_NS), 'head');
    
        DATA($self->{head_element}, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($self->{head_element}, manakai_source_column => $token->{column})
            if defined $token->{column};
      
          $self->{open_elements}->[-1]->[0]->appendChild ($self->{head_element});
          push @{$self->{open_elements}},
              [$self->{head_element}, $el_category->{head}];

          $self->{insertion_mode} = IN_HEAD_IM;
          ## Reprocess in the "in head" insertion mode...
        } else {
          
        }

        if ($token->{tag_name} eq 'base') {
          if ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
            
            ## As if </noscript>
            pop @{$self->{open_elements}};
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript', text => 'base',
                            token => $token);
          
            $self->{insertion_mode} = IN_HEAD_IM;
            ## Reprocess in the "in head" insertion mode...
          } else {
            
          }

          ## NOTE: There is a "as if in head" code clone.
          if ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after head',
                            text => $token->{tag_name}, token => $token);
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];
            $self->{head_element_inserted} = 1;
          } else {
            
          }
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS($attr);
        }
      
        DATA($el,manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el,manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          pop @{$self->{open_elements}};
          pop @{$self->{open_elements}} # <head>
              if $self->{insertion_mode} == AFTER_HEAD_IM;
          
          $token = $self->_get_next_token;
          next B;
        } elsif ($token->{tag_name} eq 'link') {
          ## NOTE: There is a "as if in head" code clone.
          if ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after head',
                            text => $token->{tag_name}, token => $token);
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];
            $self->{head_element_inserted} = 1;
          } else {
            
          }
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          pop @{$self->{open_elements}};
          pop @{$self->{open_elements}} # <head>
              if $self->{insertion_mode} == AFTER_HEAD_IM;
          delete $self->{self_closing};
          $token = $self->_get_next_token;
          next B;
        } elsif ($token->{tag_name} eq 'command') {
          if ($self->{insertion_mode} == IN_HEAD_IM) {
            ## NOTE: If the insertion mode at the time of the emission
            ## of the token was "before head", $self->{insertion_mode}
            ## is already changed to |IN_HEAD_IM|.

            ## NOTE: There is a "as if in head" code clone.
            
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
            pop @{$self->{open_elements}};
            pop @{$self->{open_elements}} # <head>
                if $self->{insertion_mode} == AFTER_HEAD_IM;
            delete $self->{self_closing};
            $token = $self->_get_next_token;
            next B;
          } else {
            ## NOTE: "in head noscript" or "after head" insertion mode
            ## - in these cases, these tags are treated as same as
            ## normal in-body tags.
            
            #
          }
        } elsif ($token->{tag_name} eq 'meta') {
          ## NOTE: There is a "as if in head" code clone.
          if ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after head',
                            text => $token->{tag_name}, token => $token);
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];
            $self->{head_element_inserted} = 1;
          } else {
            
          }
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          my $meta_el = pop @{$self->{open_elements}};

              unless ($self->{confident}) {
                if ($token->{attributes}->{charset}) {
                  
                  ## NOTE: Whether the encoding is supported or not is handled
                  ## in the {change_encoding} callback.
                  $self->{change_encoding}
                      ->($self, $token->{attributes}->{charset}->{value},
                         $token);
                  
                  DATA($meta_el->[0]->getAttributeNodeNS (undef, 'charset'),
                      manakai_has_reference => $token->{attributes}->{charset}->{has_reference});
							 
                } elsif ($token->{attributes}->{content}) {
                  if ($token->{attributes}->{content}->{value}
                      =~ /[Cc][Hh][Aa][Rr][Ss][Ee][Tt]
                          [\x09\x0A\x0C\x0D\x20]*=
                          [\x09\x0A\x0C\x0D\x20]*(?>"([^"]*)"|'([^']*)'|
                          ([^"'\x09\x0A\x0C\x0D\x20]
                           [^\x09\x0A\x0C\x0D\x20\x3B]*))/x) {
                    
                    ## NOTE: Whether the encoding is supported or not is handled
                    ## in the {change_encoding} callback.
                    $self->{change_encoding}
                        ->($self, defined $1 ? $1 : defined $2 ? $2 : $3,
                           $token);
                    DATA($meta_el->[0]->getAttributeNodeNS (undef, 'content'),
                        manakai_has_reference => $token->{attributes}->{content}->{has_reference});
                  } else {
                    
                  }
                }
              } else {
                if ($token->{attributes}->{charset}) {
                  
                  DATA($meta_el->[0]->getAttributeNodeNS(undef, 'charset'),
                      manakai_has_reference => $token->{attributes}->{charset}->{has_reference});
                }
                if ($token->{attributes}->{content}) {
                  
                  DATA($meta_el->[0]->getAttributeNodeNS(undef, 'content'),
                      manakai_has_reference => $token->{attributes}->{content}->{has_reference});
                }
              }

              pop @{$self->{open_elements}} # <head>
                  if $self->{insertion_mode} == AFTER_HEAD_IM;
              delete $self->{self_closing};
              $token = $self->_get_next_token;
              next B;
        } elsif ($token->{tag_name} eq 'title') {
          if ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
            
            ## As if </noscript>
            pop @{$self->{open_elements}};
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript', text => 'title',
                            token => $token);
          
            $self->{insertion_mode} = IN_HEAD_IM;
            ## Reprocess in the "in head" insertion mode...
          } elsif ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after head',
                            text => $token->{tag_name}, token => $token);
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];
            $self->{head_element_inserted} = 1;
          } else {
            
          }

          ## NOTE: There is a "as if in head" code clone.
          $parse_rcdata->($self, $insert, $open_tables, 1); # RCDATA

          ## NOTE: At this point the stack of open elements contain
          ## the |head| element (index == -2) and the |script| element
          ## (index == -1).  In the "after head" insertion mode the
          ## |head| element is inserted only for the purpose of
          ## providing the context for the |script| element, and
          ## therefore we can now and have to remove the element from
          ## the stack.
          splice @{$self->{open_elements}}, -2, 1, () # <head>
              if ($self->{insertion_mode} & IM_MASK) == AFTER_HEAD_IM;
          next B;
        } elsif ($token->{tag_name} eq 'style' or
                 $token->{tag_name} eq 'noframes') {
          ## NOTE: Or (scripting is enabled and tag_name eq 'noscript' and
          ## insertion mode IN_HEAD_IM)
          ## NOTE: There is a "as if in head" code clone.
          if ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after head',
                            text => $token->{tag_name}, token => $token);
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];
            $self->{head_element_inserted} = 1;
          } else {
            
          }
          $parse_rcdata->($self, $insert, $open_tables, 0); # RAWTEXT
          splice @{$self->{open_elements}}, -2, 1, () # <head>
              if ($self->{insertion_mode} & IM_MASK) == AFTER_HEAD_IM;
          next B;
        } elsif ($token->{tag_name} eq 'noscript') {
              if ($self->{insertion_mode} == IN_HEAD_IM) {
                
                ## NOTE: and scripting is disalbed
                
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
                $self->{insertion_mode} = IN_HEAD_NOSCRIPT_IM;
                
                $token = $self->_get_next_token;
                next B;
              } elsif ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript', text => 'noscript',
                                token => $token);
                ## Ignore the token
                
                $token = $self->_get_next_token;
                next B;
              } else {
                
                #
              }
        } elsif ($token->{tag_name} eq 'script') {
          if ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
            
            ## As if </noscript>
            pop @{$self->{open_elements}};
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript', text => 'script',
                            token => $token);
          
            $self->{insertion_mode} = IN_HEAD_IM;
            ## Reprocess in the "in head" insertion mode...
          } elsif ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after head',
                            text => $token->{tag_name}, token => $token);
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];
            $self->{head_element_inserted} = 1;
          } else {
            
          }

          ## NOTE: There is a "as if in head" code clone.
          $script_start_tag->($self, $insert, $open_tables);
          ## ISSUE: A spec bug  [Bug 6038]
          splice @{$self->{open_elements}}, -2, 1 # <head>
              if ($self->{insertion_mode} & IM_MASK) == AFTER_HEAD_IM;
          next B;
        } elsif ($token->{tag_name} eq 'body' or
                 $token->{tag_name} eq 'frameset') {
          if ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
            
            ## As if </noscript>
            pop @{$self->{open_elements}};
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript',
                            text => $token->{tag_name}, token => $token);
            
            ## Reprocess in the "in head" insertion mode...
            ## As if </head>
            pop @{$self->{open_elements}};
            
            ## Reprocess in the "after head" insertion mode...
          } elsif ($self->{insertion_mode} == IN_HEAD_IM) {
            
            pop @{$self->{open_elements}};
            
            ## Reprocess in the "after head" insertion mode...
          } else {
            
          }

          ## "after head" insertion mode
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          if ($token->{tag_name} eq 'body') {
            
            delete $self->{frameset_ok};
            $self->{insertion_mode} = IN_BODY_IM;
          } elsif ($token->{tag_name} eq 'frameset') {
            
            $self->{insertion_mode} = IN_FRAMESET_IM;
          } else {
            die "$0: tag name: $self->{tag_name}";
          }
          
          $token = $self->_get_next_token;
          next B;
        } else {
          
          #
        }

            if ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
              
              ## As if </noscript>
              pop @{$self->{open_elements}};
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript:/',
                              text => $token->{tag_name}, token => $token);
              
              ## Reprocess in the "in head" insertion mode...
              ## As if </head>
              pop @{$self->{open_elements}};

              ## Reprocess in the "after head" insertion mode...
            } elsif ($self->{insertion_mode} == IN_HEAD_IM) {
              
              ## As if </head>
              pop @{$self->{open_elements}};

              ## Reprocess in the "after head" insertion mode...
            } else {
              
            }

        ## "after head" insertion mode
        ## As if <body>
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), 'body');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{'body'} || 0];
    }
  
        $self->{insertion_mode} = IN_BODY_IM;
        ## The "frameset-ok" flag is not changed in this case.
        ## Reprocess the token.
        
        next B;
      } elsif ($token->{type} == END_TAG_TOKEN) {
        ## "Before head", "in head", and "after head" insertion modes
        ## ignore most of end tags.  Exceptions are "body", "html",
        ## and "br" end tags.  "Before head" and "in head" insertion
        ## modes also recognize "head" end tag.  "In head noscript"
        ## insertion modes ignore end tags except for "noscript" and
        ## "br".

        if ($token->{tag_name} eq 'head') {
          if ($self->{insertion_mode} == BEFORE_HEAD_IM) {
            
            ## As if <head>
            
      $self->{head_element} = $self->{document}->createElementNS((HTML_NS), 'head');
    
        DATA($self->{head_element}, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($self->{head_element}, manakai_source_column => $token->{column})
            if defined $token->{column};
      
            $self->{open_elements}->[-1]->[0]->appendChild($self->{head_element});
            push @{$self->{open_elements}},
                [$self->{head_element}, $el_category->{head}];

            ## Reprocess in the "in head" insertion mode...
            pop @{$self->{open_elements}};
            $self->{insertion_mode} = AFTER_HEAD_IM;
            $token = $self->_get_next_token;
            next B;
          } elsif ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
            
            #
          } elsif ($self->{insertion_mode} == IN_HEAD_IM) {
            
            pop @{$self->{open_elements}};
            $self->{insertion_mode} = AFTER_HEAD_IM;
            $token = $self->_get_next_token;
            next B;
          } elsif ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            #
          } else {
            die "$0: $self->{insertion_mode}: Unknown insertion mode";
          }
        } elsif ($token->{tag_name} eq 'noscript') {
          if ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
            
            pop @{$self->{open_elements}};
            $self->{insertion_mode} = IN_HEAD_IM;
            $token = $self->_get_next_token;
            next B;
          } else {
            
            #
          }
        } elsif ({
            body => ($self->{insertion_mode} != IN_HEAD_NOSCRIPT_IM),
            html => ($self->{insertion_mode} != IN_HEAD_NOSCRIPT_IM),
            br => 1,
        }->{$token->{tag_name}}) {
          if ($self->{insertion_mode} == BEFORE_HEAD_IM) {
            
            ## (before head) as if <head>, (in head) as if </head>
            
      $self->{head_element} = $self->{document}->createElementNS((HTML_NS), 'head');
    
        DATA($self->{head_element}, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($self->{head_element}, manakai_source_column => $token->{column})
            if defined $token->{column};
      
            $self->{open_elements}->[-1]->[0]->appendChild ($self->{head_element});
            $self->{insertion_mode} = AFTER_HEAD_IM;
  
            ## Reprocess in the "after head" insertion mode...
          } elsif ($self->{insertion_mode} == IN_HEAD_IM) {
            
            ## As if </head>
            pop @{$self->{open_elements}};
            $self->{insertion_mode} = AFTER_HEAD_IM;
  
            ## Reprocess in the "after head" insertion mode...
          } elsif ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
            
            ## NOTE: Two parse errors for <head><noscript></br>
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => $token->{tag_name}, token => $token);
            ## As if </noscript>
            pop @{$self->{open_elements}};
            $self->{insertion_mode} = IN_HEAD_IM;

            ## Reprocess in the "in head" insertion mode...
            ## As if </head>
            pop @{$self->{open_elements}};
            $self->{insertion_mode} = AFTER_HEAD_IM;

            ## Reprocess in the "after head" insertion mode...
          } elsif ($self->{insertion_mode} == AFTER_HEAD_IM) {
            
            #
          } else {
            die "$0: $self->{insertion_mode}: Unknown insertion mode";
          }

          ## "after head" insertion mode
          ## As if <body>
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), 'body');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{'body'} || 0];
    }
  
          $self->{insertion_mode} = IN_BODY_IM;
          ## The "frameset-ok" flag is left unchanged in this case.
          ## Reprocess the token.
          next B;
        }

        ## End tags are ignored by default.
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                        text => $token->{tag_name}, token => $token);
        ## Ignore the token.
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        if ($self->{insertion_mode} == BEFORE_HEAD_IM) {
          

          ## NOTE: As if <head>
          
      $self->{head_element} = $self->{document}->createElementNS((HTML_NS), 'head');
    
        DATA($self->{head_element}, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($self->{head_element}, manakai_source_column => $token->{column})
            if defined $token->{column};
      
          $self->{open_elements}->[-1]->[0]->appendChild($self->{head_element});
			 
          #push @{$self->{open_elements}},
          #    [$self->{head_element}, $el_category->{head}];
          #$self->{insertion_mode} = IN_HEAD_IM;
          ## NOTE: Reprocess.

          ## NOTE: As if </head>
          #pop @{$self->{open_elements}};
          #$self->{insertion_mode} = IN_AFTER_HEAD_IM;
          ## NOTE: Reprocess.
          
          #
        } elsif ($self->{insertion_mode} == IN_HEAD_IM) {
          

          ## NOTE: As if </head>
          pop @{$self->{open_elements}};
          #$self->{insertion_mode} = IN_AFTER_HEAD_IM;
          ## NOTE: Reprocess.

          #
        } elsif ($self->{insertion_mode} == IN_HEAD_NOSCRIPT_IM) {
          

          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in noscript:#eof', token => $token);

          ## As if </noscript>
          pop @{$self->{open_elements}};
          #$self->{insertion_mode} = IN_HEAD_IM;
          ## NOTE: Reprocess.

          ## NOTE: As if </head>
          pop @{$self->{open_elements}};
          #$self->{insertion_mode} = IN_AFTER_HEAD_IM;
          ## NOTE: Reprocess.

          #
        } else {
          
          #
        }

        ## NOTE: As if <body>
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), 'body');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{'body'} || 0];
    }
  
        $self->{insertion_mode} = IN_BODY_IM;
        ## The "frameset-ok" flag is left unchanged in this case.
        ## Reprocess the token.
        next B;
      } else {
        die "$0: $token->{type}: Unknown token type";
      }
    } elsif ($self->{insertion_mode} & BODY_IMS) {
      if ($token->{type} == CHARACTER_TOKEN) {
        
        $reconstruct_active_formatting_elements
			->($self, $insert_to_current, $active_formatting_elements, $open_tables);
        
        $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $token->{data});

        if ($token->{data} =~ /[^\x09\x0A\x0C\x0D\x20]/) {
          delete $self->{frameset_ok};
        }

        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{type} == START_TAG_TOKEN) {
            if ({
                 caption => 1, col => 1, colgroup => 1, tbody => 1,
                 td => 1, tfoot => 1, th => 1, thead => 1, tr => 1,
                }->{$token->{tag_name}}) {
              if (($self->{insertion_mode} & IM_MASK) == IN_CELL_IM) {
                ## have an element in table scope
                for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[1] == TABLE_CELL_EL) {
                    

                    ## Close the cell
                    
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <x>
                    $token = {type => END_TAG_TOKEN,
                              tag_name => $node->[0]->tagName,
                              line => $token->{line},
                              column => $token->{column}};
                    next B;
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    
                    ## ISSUE: This case can never be reached, maybe.
                    last;
                  }
                }

                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'start tag not allowed',
                    text => $token->{tag_name}, token => $token);
                ## Ignore the token
                
                $token = $self->_get_next_token;
                next B;
              } elsif (($self->{insertion_mode} & IM_MASK) == IN_CAPTION_IM) {
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed', text => 'caption',
                                token => $token);
                
                ## NOTE: As if </caption>.
                ## have a table element in table scope
                my $i;
                INSCOPE: {
                  for (reverse 0..$#{$self->{open_elements}}) {
                    my $node = $self->{open_elements}->[$_];
                    if ($node->[1] == CAPTION_EL) {
                      
                      $i = $_;
                      last INSCOPE;
                    } elsif ($node->[1] & TABLE_SCOPING_EL) {
                      
                      last;
                    }
                  }

                  
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'start tag not allowed',
                                  text => $token->{tag_name}, token => $token);
                  ## Ignore the token
                  
                  $token = $self->_get_next_token;
                  next B;
                } # INSCOPE
                
                ## generate implied end tags
                while ($self->{open_elements}->[-1]->[1]
                           & END_TAG_OPTIONAL_EL) {
                  
                  pop @{$self->{open_elements}};
                }

                unless ($self->{open_elements}->[-1]->[1] == CAPTION_EL) {
                  
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                                  text => $self->{open_elements}->[-1]->[0]->tagName,
                                  token => $token);
                } else {
                  
                }
                
                splice @{$self->{open_elements}}, $i;
                
                $clear_up_to_marker->($active_formatting_elements);
                
                $self->{insertion_mode} = IN_TABLE_IM;
                
                ## reprocess
                
                next B;
              } else {
                
                #
              }
            } else {
              
              #
            }
          } elsif ($token->{type} == END_TAG_TOKEN) {
            if ($token->{tag_name} eq 'td' or $token->{tag_name} eq 'th') {
              if (($self->{insertion_mode} & IM_MASK) == IN_CELL_IM) {
                ## have an element in table scope
                my $i;
                INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[0]->tagName eq $token->{tag_name}) {
                    
                    $i = $_;
                    last INSCOPE;
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    
                    last INSCOPE;
                  }
                } # INSCOPE
                  unless (defined $i) {
                    
                    $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                    text => $token->{tag_name},
                                    token => $token);
                    ## Ignore the token
                    $token = $self->_get_next_token;
                    next B;
                  }
                
                ## generate implied end tags
                while ($self->{open_elements}->[-1]->[1]
                           & END_TAG_OPTIONAL_EL) {
                  
                  pop @{$self->{open_elements}};
                }

                if ($self->{open_elements}->[-1]->[0]->tagName
                        ne $token->{tag_name}) {
                  
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                                  text => $self->{open_elements}->[-1]->[0]->tagName,
                                  token => $token);
                } else {
                  
                }
                
                splice @{$self->{open_elements}}, $i;
                
                $clear_up_to_marker->($active_formatting_elements);
                
                $self->{insertion_mode} = IN_ROW_IM;
                
                $token = $self->_get_next_token;
                next B;
              } elsif (($self->{insertion_mode} & IM_MASK) == IN_CAPTION_IM) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => $token->{tag_name}, token => $token);
                ## Ignore the token
                $token = $self->_get_next_token;
                next B;
              } else {
                
                #
              }
            } elsif ($token->{tag_name} eq 'caption') {
              if (($self->{insertion_mode} & IM_MASK) == IN_CAPTION_IM) {
                ## have a table element in table scope
                my $i;
                INSCOPE: {
                  for (reverse 0..$#{$self->{open_elements}}) {
                    my $node = $self->{open_elements}->[$_];
                    if ($node->[1] == CAPTION_EL) {
                      
                      $i = $_;
                      last INSCOPE;
                    } elsif ($node->[1] & TABLE_SCOPING_EL) {
                      
                      last;
                    }
                  }

                  
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                  text => $token->{tag_name}, token => $token);
                  ## Ignore the token
                  $token = $self->_get_next_token;
                  next B;
                } # INSCOPE
                
                ## generate implied end tags
                while ($self->{open_elements}->[-1]->[1]
                           & END_TAG_OPTIONAL_EL) {
                  
                  pop @{$self->{open_elements}};
                }
                
                unless ($self->{open_elements}->[-1]->[1] == CAPTION_EL) {
                  
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                                  text => $self->{open_elements}->[-1]->[0]->tagName,
                                  token => $token);
                } else {
                  
                }
                
                splice @{$self->{open_elements}}, $i;
                
                $clear_up_to_marker->($active_formatting_elements);
                
                $self->{insertion_mode} = IN_TABLE_IM;
                
                $token = $self->_get_next_token;
                next B;
              } elsif (($self->{insertion_mode} & IM_MASK) == IN_CELL_IM) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => $token->{tag_name}, token => $token);
                ## Ignore the token
                $token = $self->_get_next_token;
                next B;
              } else {
                
                #
              }
            } elsif ({
                      table => 1, tbody => 1, tfoot => 1, 
                      thead => 1, tr => 1,
                     }->{$token->{tag_name}} and
                     ($self->{insertion_mode} & IM_MASK) == IN_CELL_IM) {
              ## have an element in table scope
              my $i;
              my $tn;
              INSCOPE: {
                for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[0]->tagName eq $token->{tag_name}) {
                    
                    $i = $_;

                    ## Close the cell
                    
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # </x>
                    $token = {type => END_TAG_TOKEN, tag_name => $tn,
                              line => $token->{line},
                              column => $token->{column}};
                    next B;
                  } elsif ($node->[1] == TABLE_CELL_EL) {
                    
                    $tn = $node->[0]->tagName;
                    ## NOTE: There is exactly one |td| or |th| element
                    ## in scope in the stack of open elements by definition.
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    ## ISSUE: Can this be reached?
                    
                    last;
                  }
                }

                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                    text => $token->{tag_name}, token => $token);
                ## Ignore the token
                $token = $self->_get_next_token;
                next B;
              } # INSCOPE
            } elsif ($token->{tag_name} eq 'table' and
                     ($self->{insertion_mode} & IM_MASK) == IN_CAPTION_IM) {
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed', text => 'caption',
                              token => $token);

              ## As if </caption>
              ## have a table element in table scope
              my $i;
              INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                my $node = $self->{open_elements}->[$_];
                if ($node->[1] == CAPTION_EL) {
                  
                  $i = $_;
                  last INSCOPE;
                } elsif ($node->[1] & TABLE_SCOPING_EL) {
                  
                  last INSCOPE;
                }
              } # INSCOPE
              unless (defined $i) {
                
	## TODO: Wrong error type?
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => 'caption', token => $token);
                ## Ignore the token
                $token = $self->_get_next_token;
                next B;
              }
              
              ## generate implied end tags
              while ($self->{open_elements}->[-1]->[1] & END_TAG_OPTIONAL_EL) {
                
                pop @{$self->{open_elements}};
              }

              unless ($self->{open_elements}->[-1]->[1] == CAPTION_EL) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                                text => $self->{open_elements}->[-1]->[0]
                                    ->tagName,
                                token => $token);
              } else {
                
              }

              splice @{$self->{open_elements}}, $i;

              $clear_up_to_marker->($active_formatting_elements);

              $self->{insertion_mode} = IN_TABLE_IM;

              ## reprocess
              next B;
            } elsif ({
                      body => 1, col => 1, colgroup => 1, html => 1,
                     }->{$token->{tag_name}}) {
              if ($self->{insertion_mode} & BODY_TABLE_IMS) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => $token->{tag_name}, token => $token);
                ## Ignore the token
                $token = $self->_get_next_token;
                next B;
              } else {
                
                #
              }
        } elsif ({
                  tbody => 1, tfoot => 1,
                  thead => 1, tr => 1,
                 }->{$token->{tag_name}} and
                 ($self->{insertion_mode} & IM_MASK) == IN_CAPTION_IM) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);
          ## Ignore the token
          $token = $self->_get_next_token;
          next B;
        } else {
          
          #
        }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        for my $entry (@{$self->{open_elements}}) {
          unless ($entry->[1] & ALL_END_TAG_OPTIONAL_EL) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in body:#eof', token => $token);
            last;
          }
        }

        ## Stop parsing.
        last B;
      } else {
        die "$0: $token->{type}: Unknown token type";
      }

      $insert = $insert_to_current;
      #
    } elsif ($self->{insertion_mode} & TABLE_IMS) {
      if ($token->{type} == START_TAG_TOKEN) {
        if ({
             tr => (($self->{insertion_mode} & IM_MASK) != IN_ROW_IM),
             th => 1, td => 1,
            }->{$token->{tag_name}}) {
          if (($self->{insertion_mode} & IM_MASK) == IN_TABLE_IM) {
            ## Clear back to table context
            while (not ($self->{open_elements}->[-1]->[1]
                            & TABLE_SCOPING_EL)) {
              
              pop @{$self->{open_elements}};
            }
            
            
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), 'tbody');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{'tbody'} || 0];
    }
  
            $self->{insertion_mode} = IN_TABLE_BODY_IM;
            ## reprocess in the "in table body" insertion mode...
          }
          
          if (($self->{insertion_mode} & IM_MASK) == IN_TABLE_BODY_IM) {
            unless ($token->{tag_name} eq 'tr') {
              
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'missing start tag:tr', token => $token);
            }
                
            ## Clear back to table body context
            while (not ($self->{open_elements}->[-1]->[1]
                            & TABLE_ROWS_SCOPING_EL)) {
              
              ## ISSUE: Can this case be reached?
              pop @{$self->{open_elements}};
            }
                
            $self->{insertion_mode} = IN_ROW_IM;
            if ($token->{tag_name} eq 'tr') {
              
              
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
              $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted
              
              $token = $self->_get_next_token;
              next B;
            } else {
              
              
    {
      my $el;
      
      $el = $self->{document}->createElementNS
        ((HTML_NS), 'tr');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{'tr'} || 0];
    }
  
              ## reprocess in the "in row" insertion mode
            }
          } else {
            
          }

              ## Clear back to table row context
              while (not ($self->{open_elements}->[-1]->[1]
                              & TABLE_ROW_SCOPING_EL)) {
                
                pop @{$self->{open_elements}};
              }
              
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted
          $self->{insertion_mode} = IN_CELL_IM;

          push @$active_formatting_elements, ['#marker', '', undef];
              
          
          $token = $self->_get_next_token;
          next B;
        } elsif ({
                  caption => 1, col => 1, colgroup => 1,
                  tbody => 1, tfoot => 1, thead => 1,
                  tr => 1, # $self->{insertion_mode} == IN_ROW_IM
                 }->{$token->{tag_name}}) {
          if (($self->{insertion_mode} & IM_MASK) == IN_ROW_IM) {
            ## As if </tr>
            ## have an element in table scope
            my $i;
            INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
              my $node = $self->{open_elements}->[$_];
              if ($node->[1] == TABLE_ROW_EL) {
                
                $i = $_;
                last INSCOPE;
              } elsif ($node->[1] & TABLE_SCOPING_EL) {
                
                last INSCOPE;
              }
            } # INSCOPE
            unless (defined $i) { 
              
              ## TODO: This type is wrong.
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmacthed end tag',
                              text => $token->{tag_name}, token => $token);
              ## Ignore the token
              
              $token = $self->_get_next_token;
              next B;
            }
                
                ## Clear back to table row context
                while (not ($self->{open_elements}->[-1]->[1]
                                & TABLE_ROW_SCOPING_EL)) {
                  
                  ## ISSUE: Can this case be reached?
                  pop @{$self->{open_elements}};
                }
                
                pop @{$self->{open_elements}}; # tr
                $self->{insertion_mode} = IN_TABLE_BODY_IM;
                if ($token->{tag_name} eq 'tr') {
                  
                  ## reprocess
                  
                  next B;
                } else {
                  
                  ## reprocess in the "in table body" insertion mode...
                }
              }

              if (($self->{insertion_mode} & IM_MASK) == IN_TABLE_BODY_IM) {
                ## have an element in table scope
                my $i;
                INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[1] == TABLE_ROW_GROUP_EL) {
                    
                    $i = $_;
                    last INSCOPE;
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    
                    last INSCOPE;
                  }
                } # INSCOPE
                unless (defined $i) {
                  
## TODO: This erorr type is wrong.
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                  text => $token->{tag_name}, token => $token);
                  ## Ignore the token
                  
                  $token = $self->_get_next_token;
                  next B;
                }

                ## Clear back to table body context
                while (not ($self->{open_elements}->[-1]->[1]
                                & TABLE_ROWS_SCOPING_EL)) {
                  
                  ## ISSUE: Can this state be reached?
                  pop @{$self->{open_elements}};
                }
                
                ## As if <{current node}>
                ## have an element in table scope
                ## true by definition
                
                ## Clear back to table body context
                ## nop by definition
                
                pop @{$self->{open_elements}};
                $self->{insertion_mode} = IN_TABLE_IM;
                ## reprocess in "in table" insertion mode...
              } else {
                
              }

          if ($token->{tag_name} eq 'col') {
            ## Clear back to table context
            while (not ($self->{open_elements}->[-1]->[1]
                            & TABLE_SCOPING_EL)) {
              
              ## ISSUE: Can this state be reached?
              pop @{$self->{open_elements}};
            }
            
            
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), 'colgroup');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{'colgroup'} || 0];
    }
  
            $self->{insertion_mode} = IN_COLUMN_GROUP_IM;
            ## reprocess
            $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted
            
            next B;
          } elsif ({
                    caption => 1,
                    colgroup => 1,
                    tbody => 1, tfoot => 1, thead => 1,
                   }->{$token->{tag_name}}) {
            ## Clear back to table context
                while (not ($self->{open_elements}->[-1]->[1]
                                & TABLE_SCOPING_EL)) {
                  
                  ## ISSUE: Can this state be reached?
                  pop @{$self->{open_elements}};
                }
                
            push @$active_formatting_elements, ['#marker', '', undef]
                if $token->{tag_name} eq 'caption';
                
            
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
            $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted
            $self->{insertion_mode} = {
                                       caption => IN_CAPTION_IM,
                                       colgroup => IN_COLUMN_GROUP_IM,
                                       tbody => IN_TABLE_BODY_IM,
                                       tfoot => IN_TABLE_BODY_IM,
                                       thead => IN_TABLE_BODY_IM,
                                      }->{$token->{tag_name}};
            $token = $self->_get_next_token;
            
            next B;
          } else {
            die "$0: in table: <>: $token->{tag_name}";
          }
            } elsif ($token->{tag_name} eq 'table') {
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                              text => $self->{open_elements}->[-1]->[0]
                                  ->tagName,
                              token => $token);

              ## As if </table>
              ## have a table element in table scope
              my $i;
              INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                my $node = $self->{open_elements}->[$_];
                if ($node->[1] == TABLE_EL) {
                  
                  $i = $_;
                  last INSCOPE;
                } elsif ($node->[1] & TABLE_SCOPING_EL) {
                  
                  last INSCOPE;
                }
              } # INSCOPE
              unless (defined $i) {
                
## TODO: The following is wrong, maybe.
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag', text => 'table',
                                token => $token);
                ## Ignore tokens </table><table>
                
                $token = $self->_get_next_token;
                next B;
              }
              
## TODO: Followings are removed from the latest spec. 
              ## generate implied end tags
              while ($self->{open_elements}->[-1]->[1] & END_TAG_OPTIONAL_EL) {
                
                pop @{$self->{open_elements}};
              }

              unless ($self->{open_elements}->[-1]->[1] == TABLE_EL) {
                
                ## NOTE: |<table><tr><table>|
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                                text => $self->{open_elements}->[-1]->[0]
                                    ->tagName,
                                token => $token);
              } else {
                
              }

              splice @{$self->{open_elements}}, $i;
              pop @{$open_tables};

              $self->_reset_insertion_mode; 

          ## reprocess
          
          next B;
        } elsif ($token->{tag_name} eq 'style') {
          
          ## NOTE: This is a "as if in head" code clone.
          $parse_rcdata->($self, $insert, $open_tables, 0); # RAWTEXT
          $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted
          next B;
        } elsif ($token->{tag_name} eq 'script') {
          
          ## NOTE: This is a "as if in head" code clone.
          $script_start_tag->($self, $insert, $open_tables);
          $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted
          next B;
        } elsif ($token->{tag_name} eq 'input') {
          if ($token->{attributes}->{type}) {
            my $type = $token->{attributes}->{type}->{value};
            $type =~ tr/A-Z/a-z/; ## ASCII case-insensitive.
            if ($type eq 'hidden') {
              
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'in table',
                              text => $token->{tag_name}, token => $token);

              
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
              $open_tables->[-1]->[2] = 0 if @$open_tables; # ~node inserted

              ## TODO: form element pointer

              pop @{$self->{open_elements}};

              $token = $self->_get_next_token;
              delete $self->{self_closing};
              next B;
            } else {
              
              #
            }
          } else {
            
            #
          }
        } elsif ($token->{tag_name} eq 'form') {
         $self->{parse_error}->(level => $self->{level}->{must}, type => 'form in table', token => $token); # XXX documentation
           
         if ($self->{form_element}) {
           ## Ignore the token.
           $token = $self->_get_next_token;
             
           next B;
           } else {
             
     {
       my $el;
       
       $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
     
         for my $attr_name (keys %{  $token->{attributes}}) {
           my $attr_t =   $token->{attributes}->{$attr_name};
           my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
           $attr->setValue($attr_t->{value});
           DATA($attr, manakai_source_line => $attr_t->{line});
           DATA($attr, manakai_source_column => $attr_t->{column});
           $el->setAttributeNodeNS($attr);
         }
       
         DATA($el, manakai_source_line => $token->{line})
             if defined $token->{line};
         DATA($el, manakai_source_column => $token->{column})
             if defined $token->{column};
       
       $self->{open_elements}->[-1]->[0]->appendChild ($el);
       push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
     }
   
             $self->{form_element} = $self->{open_elements}->[-1]->[0];
             
             pop @{$self->{open_elements}};
             
             $token = $self->_get_next_token;
             
             next B;
           }
        } else {
          
          #
        }

        $self->{parse_error}->(level => $self->{level}->{must}, type => 'in table', text => $token->{tag_name},
                        token => $token);

        $insert = $insert_to_foster;
        #
      } elsif ($token->{type} == END_TAG_TOKEN) {
        if ($token->{tag_name} eq 'tr' and
            ($self->{insertion_mode} & IM_MASK) == IN_ROW_IM) {
          ## have an element in table scope
              my $i;
              INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                my $node = $self->{open_elements}->[$_];
                if ($node->[1] == TABLE_ROW_EL) {
                  
                  $i = $_;
                  last INSCOPE;
                } elsif ($node->[1] & TABLE_SCOPING_EL) {
                  
                  last INSCOPE;
                }
              } # INSCOPE
              unless (defined $i) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => $token->{tag_name}, token => $token);
                ## Ignore the token
                
                $token = $self->_get_next_token;
                next B;
              } else {
                
              }

              ## Clear back to table row context
              while (not ($self->{open_elements}->[-1]->[1]
                              & TABLE_ROW_SCOPING_EL)) {
                
## ISSUE: Can this state be reached?
                pop @{$self->{open_elements}};
              }

              pop @{$self->{open_elements}}; # tr
              $self->{insertion_mode} = IN_TABLE_BODY_IM;
              $token = $self->_get_next_token;
              
              next B;
            } elsif ($token->{tag_name} eq 'table') {
              if (($self->{insertion_mode} & IM_MASK) == IN_ROW_IM) {
                ## As if </tr>
                ## have an element in table scope
                my $i;
                INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[1] == TABLE_ROW_EL) {
                    
                    $i = $_;
                    last INSCOPE;
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    
                    last INSCOPE;
                  }
                } # INSCOPE
                unless (defined $i) {
                  
## TODO: The following is wrong.
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                  text => $token->{type}, token => $token);
                  ## Ignore the token
                  
                  $token = $self->_get_next_token;
                  next B;
                }
                
                ## Clear back to table row context
                while (not ($self->{open_elements}->[-1]->[1]
                                & TABLE_ROW_SCOPING_EL)) {
                  
## ISSUE: Can this state be reached?
                  pop @{$self->{open_elements}};
                }
                
                pop @{$self->{open_elements}}; # tr
                $self->{insertion_mode} = IN_TABLE_BODY_IM;
                ## reprocess in the "in table body" insertion mode...
              }

              if (($self->{insertion_mode} & IM_MASK) == IN_TABLE_BODY_IM) {
                ## have an element in table scope
                my $i;
                INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[1] == TABLE_ROW_GROUP_EL) {
                    
                    $i = $_;
                    last INSCOPE;
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    
                    last INSCOPE;
                  }
                } # INSCOPE
                unless (defined $i) {
                  
                  $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                  text => $token->{tag_name}, token => $token);
                  ## Ignore the token
                  
                  $token = $self->_get_next_token;
                  next B;
                }
                
                ## Clear back to table body context
                while (not ($self->{open_elements}->[-1]->[1]
                                & TABLE_ROWS_SCOPING_EL)) {
                  
                  pop @{$self->{open_elements}};
                }
                
                ## As if <{current node}>
                ## have an element in table scope
                ## true by definition
                
                ## Clear back to table body context
                ## nop by definition
                
                pop @{$self->{open_elements}};
                $self->{insertion_mode} = IN_TABLE_IM;
                ## reprocess in the "in table" insertion mode...
              }

              ## NOTE: </table> in the "in table" insertion mode.
              ## When you edit the code fragment below, please ensure that
              ## the code for <table> in the "in table" insertion mode
              ## is synced with it.

              ## have a table element in table scope
              my $i;
              INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                my $node = $self->{open_elements}->[$_];
                if ($node->[1] == TABLE_EL) {
                  
                  $i = $_;
                  last INSCOPE;
                } elsif ($node->[1] & TABLE_SCOPING_EL) {
                  
                  last INSCOPE;
                }
              } # INSCOPE
              unless (defined $i) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => $token->{tag_name}, token => $token);
                ## Ignore the token
                
                $token = $self->_get_next_token;
                next B;
              }
                
              splice @{$self->{open_elements}}, $i;
              pop @{$open_tables};
              
              $self->_reset_insertion_mode;
              
              $token = $self->_get_next_token;
              next B;
            } elsif ({
                      tbody => 1, tfoot => 1, thead => 1,
                     }->{$token->{tag_name}} and
                     $self->{insertion_mode} & ROW_IMS) {
              if (($self->{insertion_mode} & IM_MASK) == IN_ROW_IM) {
                ## have an element in table scope
                my $i;
                INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[0]->tagName eq $token->{tag_name}) {
                    
                    $i = $_;
                    last INSCOPE;
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    
                    last INSCOPE;
                  }
                } # INSCOPE
                  unless (defined $i) {
                    
                    $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                    text => $token->{tag_name}, token => $token);
                    ## Ignore the token
                    
                    $token = $self->_get_next_token;
                    next B;
                  }
                
                ## As if </tr>
                ## have an element in table scope
                no warnings; my $i; use warnings;
                INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                  my $node = $self->{open_elements}->[$_];
                  if ($node->[1] == TABLE_ROW_EL) {
                    
                    $i = $_;
                    last INSCOPE;
                  } elsif ($node->[1] & TABLE_SCOPING_EL) {
                    
                    last INSCOPE;
                  }
                } # INSCOPE
                  unless (defined $i) {
                    
                    $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                    text => 'tr', token => $token);
                    ## Ignore the token
                    
                    $token = $self->_get_next_token;
                    next B;
                  }
                
                ## Clear back to table row context
                while (not ($self->{open_elements}->[-1]->[1]
                                & TABLE_ROW_SCOPING_EL)) {
                  
## ISSUE: Can this case be reached?
                  pop @{$self->{open_elements}};
                }
                
                pop @{$self->{open_elements}}; # tr
                $self->{insertion_mode} = IN_TABLE_BODY_IM;
                ## reprocess in the "in table body" insertion mode...
              }

              ## have an element in table scope
              my $i;
              INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
                my $node = $self->{open_elements}->[$_];
                if ($node->[0]->tagName eq $token->{tag_name}) {
                  
                  $i = $_;
                  last INSCOPE;
                } elsif ($node->[1] & TABLE_SCOPING_EL) {
                  
                  last INSCOPE;
                }
              } # INSCOPE
              unless (defined $i) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => $token->{tag_name}, token => $token);
                ## Ignore the token
                
                $token = $self->_get_next_token;
                next B;
              }

              ## Clear back to table body context
              while (not ($self->{open_elements}->[-1]->[1]
                              & TABLE_ROWS_SCOPING_EL)) {
                
## ISSUE: Can this case be reached?
                pop @{$self->{open_elements}};
              }

              pop @{$self->{open_elements}};
              $self->{insertion_mode} = IN_TABLE_IM;
              
              $token = $self->_get_next_token;
              next B;
            } elsif ({
                      body => 1, caption => 1, col => 1, colgroup => 1,
                      html => 1, td => 1, th => 1,
                      tr => 1, # $self->{insertion_mode} == IN_ROW_IM
                      tbody => 1, tfoot => 1, thead => 1, # $self->{insertion_mode} == IN_TABLE_IM
                     }->{$token->{tag_name}}) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);
          ## Ignore the token
          
           $token = $self->_get_next_token;
          next B;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in table:/',
                          text => $token->{tag_name}, token => $token);

          $insert = $insert_to_foster;
          #
        }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        unless ($self->{open_elements}->[-1]->[1] == HTML_EL and
                @{$self->{open_elements}} == 1) { # redundant, maybe
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in body:#eof', token => $token);
          
          #
        } else {
          
          #
        }

        ## Stop parsing
        last B;
      } else {
        die "$0: $token->{type}: Unknown token type";
      }
    } elsif (($self->{insertion_mode} & IM_MASK) == IN_COLUMN_GROUP_IM) {
          if ($token->{type} == CHARACTER_TOKEN) {
            if ($token->{data} =~ s/^([\x09\x0A\x0C\x20]+)//) {
              $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $1);
              unless (length $token->{data}) {
                
                $token = $self->_get_next_token;
                next B;
              }
            }
            
            
            #
          } elsif ($token->{type} == START_TAG_TOKEN) {
            if ($token->{tag_name} eq 'col') {
              
              
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
              pop @{$self->{open_elements}};
              delete $self->{self_closing};
              $token = $self->_get_next_token;
              next B;
            } else { 
              
              #
            }
          } elsif ($token->{type} == END_TAG_TOKEN) {
            if ($token->{tag_name} eq 'colgroup') {
              if ($self->{open_elements}->[-1]->[1] == HTML_EL) {
                
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                                text => 'colgroup', token => $token);
                ## Ignore the token
                $token = $self->_get_next_token;
                next B;
              } else {
                
                pop @{$self->{open_elements}}; # colgroup
                $self->{insertion_mode} = IN_TABLE_IM;
                $token = $self->_get_next_token;
                next B;             
              }
            } elsif ($token->{tag_name} eq 'col') {
              
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                              text => 'col', token => $token);
              ## Ignore the token
              $token = $self->_get_next_token;
              next B;
            } else {
              
              # 
            }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        if ($self->{open_elements}->[-1]->[1] == HTML_EL and
            @{$self->{open_elements}} == 1) { # redundant, maybe
          
          ## Stop parsing.
          last B;
        } else {
          ## NOTE: As if </colgroup>.
          
          pop @{$self->{open_elements}}; # colgroup
          $self->{insertion_mode} = IN_TABLE_IM;
          ## Reprocess.
          next B;
        }
      } else {
        die "$0: $token->{type}: Unknown token type";
      }

          ## As if </colgroup>
          if ($self->{open_elements}->[-1]->[1] == HTML_EL) {
            
## TODO: Wrong error type?
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => 'colgroup', token => $token);
            ## Ignore the token
            
            $token = $self->_get_next_token;
            next B;
          } else {
            
            pop @{$self->{open_elements}}; # colgroup
            $self->{insertion_mode} = IN_TABLE_IM;
            
            ## reprocess
            next B;
          }
    } elsif ($self->{insertion_mode} & SELECT_IMS) {
      if ($token->{type} == CHARACTER_TOKEN) {
        
        $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $token->{data});
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{type} == START_TAG_TOKEN) {
        if ($token->{tag_name} eq 'option') {
          if ($self->{open_elements}->[-1]->[1] == OPTION_EL) {
            
            ## As if </option>
            pop @{$self->{open_elements}};
          } else {
            
          }

          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          
          $token = $self->_get_next_token;
          next B;
        } elsif ($token->{tag_name} eq 'optgroup') {
          if ($self->{open_elements}->[-1]->[1] == OPTION_EL) {
            
            ## As if </option>
            pop @{$self->{open_elements}};
          } else {
            
          }

          if ($self->{open_elements}->[-1]->[1] == OPTGROUP_EL) {
            
            ## As if </optgroup>
            pop @{$self->{open_elements}};
          } else {
            
          }

          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          
          $token = $self->_get_next_token;
          next B;
        } elsif ({
                   select => 1, input => 1, textarea => 1, keygen => 1,
                 }->{$token->{tag_name}} or
                 (($self->{insertion_mode} & IM_MASK)
                      == IN_SELECT_IN_TABLE_IM and
                  {
                   caption => 1, table => 1,
                   tbody => 1, tfoot => 1, thead => 1,
                   tr => 1, td => 1, th => 1,
                  }->{$token->{tag_name}})) {

          ## 1. Parse error.
          if ($token->{tag_name} eq 'select') {
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'select in select', ## XXX: documentation
                              token => $token);
          } else {
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed', text => 'select',
                            token => $token);
          }

          ## 2./<select>-1. Unless "have an element in table scope" (select):
          my $i;
          INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
            my $node = $self->{open_elements}->[$_];
            if ($node->[1] == SELECT_EL) {
              
              $i = $_;
              last INSCOPE;
            } elsif ($node->[1] & TABLE_SCOPING_EL) {
              
              last INSCOPE;
            }
          } # INSCOPE
          unless (defined $i) {
            
            if ($token->{tag_name} eq 'select') {
              ## NOTE: This error would be raised when
              ## |select.innerHTML = '<select>'| is executed; in this
              ## case two errors, "select in select" and "unmatched
              ## end tags" are reported to the user, the latter might
              ## be confusing but this is what the spec requires.
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                              text => 'select',
                              token => $token);
            }
            ## Ignore the token.
            
            $token = $self->_get_next_token;
            next B;
          }

          ## 3. Otherwise, as if there were <select>:
              
          
          splice @{$self->{open_elements}}, $i;

          $self->_reset_insertion_mode;

          if ($token->{tag_name} eq 'select') {
            
            $token = $self->_get_next_token;
            next B;
          } else {
            
            
            ## Reprocess the token.
            next B;
          }
        } elsif ($token->{tag_name} eq 'script') {
          
          ## NOTE: This is an "as if in head" code clone
          $script_start_tag->($self, $insert, $open_tables);
          next B;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in select',
                          text => $token->{tag_name}, token => $token);
          ## Ignore the token
          
          $token = $self->_get_next_token;
          next B;
        }
      } elsif ($token->{type} == END_TAG_TOKEN) {
        if ($token->{tag_name} eq 'optgroup') {
          if ($self->{open_elements}->[-1]->[1] == OPTION_EL and
              $self->{open_elements}->[-2]->[1] == OPTGROUP_EL) {
            
            ## As if </option>
            splice @{$self->{open_elements}}, -2;
          } elsif ($self->{open_elements}->[-1]->[1] == OPTGROUP_EL) {
            
            pop @{$self->{open_elements}};
          } else {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => $token->{tag_name}, token => $token);
            ## Ignore the token
          }
          
          $token = $self->_get_next_token;
          next B;
        } elsif ($token->{tag_name} eq 'option') {
          if ($self->{open_elements}->[-1]->[1] == OPTION_EL) {
            
            pop @{$self->{open_elements}};
          } else {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => $token->{tag_name}, token => $token);
            ## Ignore the token
          }
          
          $token = $self->_get_next_token;
          next B;
        } elsif ($token->{tag_name} eq 'select') {
          ## have an element in table scope
          my $i;
          INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
            my $node = $self->{open_elements}->[$_];
            if ($node->[1] == SELECT_EL) {
              
              $i = $_;
              last INSCOPE;
            } elsif ($node->[1] & TABLE_SCOPING_EL) {
              
              last INSCOPE;
            }
          } # INSCOPE
          unless (defined $i) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => $token->{tag_name}, token => $token);
            ## Ignore the token
            
            $token = $self->_get_next_token;
            next B;
          }
              
          
          splice @{$self->{open_elements}}, $i;

          $self->_reset_insertion_mode;

          
          $token = $self->_get_next_token;
          next B;
        } elsif (($self->{insertion_mode} & IM_MASK)
                     == IN_SELECT_IN_TABLE_IM and
                 {
                  caption => 1, table => 1, tbody => 1,
                  tfoot => 1, thead => 1, tr => 1, td => 1, th => 1,
                 }->{$token->{tag_name}}) {
## TODO: The following is wrong?
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);
              
          ## have an element in table scope
          my $i;
          INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
            my $node = $self->{open_elements}->[$_];
            if ($node->[0]->tagName eq $token->{tag_name}) {
              
              $i = $_;
              last INSCOPE;
            } elsif ($node->[1] & TABLE_SCOPING_EL) {
              
              last INSCOPE;
            }
          } # INSCOPE
          unless (defined $i) {
            
            ## Ignore the token
            
            $token = $self->_get_next_token;
            next B;
          }
              
          ## As if </select>
          ## have an element in table scope
          undef $i;
          INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
            my $node = $self->{open_elements}->[$_];
            if ($node->[1] == SELECT_EL) {
              
              $i = $_;
              last INSCOPE;
            } elsif ($node->[1] & TABLE_SCOPING_EL) {
## ISSUE: Can this state be reached?
              
              last INSCOPE;
            }
          } # INSCOPE
          unless (defined $i) {
            
## TODO: The following error type is correct?
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => 'select', token => $token);
            ## Ignore the </select> token
            
            $token = $self->_get_next_token; ## TODO: ok?
            next B;
          }
              
          
          splice @{$self->{open_elements}}, $i;

          $self->_reset_insertion_mode;

          
          ## reprocess
          next B;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in select:/',
                          text => $token->{tag_name}, token => $token);
          ## Ignore the token
          
          $token = $self->_get_next_token;
          next B;
        }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        unless ($self->{open_elements}->[-1]->[1] == HTML_EL and
                @{$self->{open_elements}} == 1) { # redundant, maybe
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in body:#eof', token => $token);
        } else {
          
        }

        ## Stop parsing.
        last B;
      } else {
        die "$0: $token->{type}: Unknown token type";
      }
    } elsif ($self->{insertion_mode} & BODY_AFTER_IMS) {
      if ($token->{type} == CHARACTER_TOKEN) {
        if ($token->{data} =~ s/^([\x09\x0A\x0C\x20]+)//) {
          my $data = $1;
          ## As if in body
          $reconstruct_active_formatting_elements
				->($self, $insert_to_current, $active_formatting_elements, $open_tables);
              
          $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $1);
          
          unless (length $token->{data}) {
            
            $token = $self->_get_next_token;
            next B;
          }
        }
        
        if ($self->{insertion_mode} == AFTER_HTML_BODY_IM) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'after html:#text', token => $token);
          #
        } else {
          
          ## "after body" insertion mode
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'after body:#text', token => $token);
          #
        }

        $self->{insertion_mode} = IN_BODY_IM;
        ## reprocess
        next B;
      } elsif ($token->{type} == START_TAG_TOKEN) {
        if ($self->{insertion_mode} == AFTER_HTML_BODY_IM) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'after html',
                          text => $token->{tag_name}, token => $token);
          #
        } else {
          
          ## "after body" insertion mode
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'after body',
                          text => $token->{tag_name}, token => $token);
          #
        }

        $self->{insertion_mode} = IN_BODY_IM;
        
        ## reprocess
        next B;
      } elsif ($token->{type} == END_TAG_TOKEN) {
        if ($self->{insertion_mode} == AFTER_HTML_BODY_IM) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'after html:/',
                          text => $token->{tag_name}, token => $token);
          
          $self->{insertion_mode} = IN_BODY_IM;
          ## Reprocess.
          next B;
        } else {
          
        }

        ## "after body" insertion mode
        if ($token->{tag_name} eq 'html') {
          if (defined $self->{inner_html_node}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => 'html', token => $token);
            ## Ignore the token
            $token = $self->_get_next_token;
            next B;
          } else {
            
            $self->{insertion_mode} = AFTER_HTML_BODY_IM;
            $token = $self->_get_next_token;
            next B;
          }
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'after body:/',
                          text => $token->{tag_name}, token => $token);

          $self->{insertion_mode} = IN_BODY_IM;
          ## reprocess
          next B;
        }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        
        ## Stop parsing
        last B;
      } else {
        die "$0: $token->{type}: Unknown token type";
      }
    } elsif ($self->{insertion_mode} & FRAME_IMS) {
      if ($token->{type} == CHARACTER_TOKEN) {
        if ($token->{data} =~ s/^([\x09\x0A\x0C\x20]+)//) {
          $self->{open_elements}->[-1]->[0]->appendTextFromUnicode($self, $1);
          
          unless (length $token->{data}) {
            
            $token = $self->_get_next_token;
            next B;
          }
        }
        
        if ($token->{data} =~ s/^[^\x09\x0A\x0C\x20]+//) {
          if ($self->{insertion_mode} == IN_FRAMESET_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in frameset:#text', token => $token);
          } elsif ($self->{insertion_mode} == AFTER_FRAMESET_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after frameset:#text', token => $token);
          } else { # "after after frameset"
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after html:#text', token => $token);
          }
          
          ## Ignore the token.
          if (length $token->{data}) {
            
            ## reprocess the rest of characters
          } else {
            
            $token = $self->_get_next_token;
          }
          next B;
        }
        
        die qq[$0: Character "$token->{data}"];
      } elsif ($token->{type} == START_TAG_TOKEN) {
        if ($token->{tag_name} eq 'frameset' and
            $self->{insertion_mode} == IN_FRAMESET_IM) {
          
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          
          $token = $self->_get_next_token;
          next B;
        } elsif ($token->{tag_name} eq 'frame' and
                 $self->{insertion_mode} == IN_FRAMESET_IM) {
          
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $self->{open_elements}->[-1]->[0]->appendChild ($el);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
          pop @{$self->{open_elements}};
          delete $self->{self_closing};
          $token = $self->_get_next_token;
          next B;
        } elsif ($token->{tag_name} eq 'noframes') {
          
          ## NOTE: As if in head.
          $parse_rcdata->($self, $insert, $open_tables, 0); # RAWTEXT
          next B;

          ## NOTE: |<!DOCTYPE HTML><frameset></frameset></html><noframes></noframes>|
          ## has no parse error.
        } else {
          if ($self->{insertion_mode} == IN_FRAMESET_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in frameset',
                            text => $token->{tag_name}, token => $token);
          } elsif ($self->{insertion_mode} == AFTER_FRAMESET_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after frameset',
                            text => $token->{tag_name}, token => $token);
          } else { # "after after frameset"
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after after frameset',
                            text => $token->{tag_name}, token => $token);
          }
          ## Ignore the token
          
          $token = $self->_get_next_token;
          next B;
        }
      } elsif ($token->{type} == END_TAG_TOKEN) {
        if ($token->{tag_name} eq 'frameset' and
            $self->{insertion_mode} == IN_FRAMESET_IM) {
          if ($self->{open_elements}->[-1]->[1] == HTML_EL and
              @{$self->{open_elements}} == 1) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => $token->{tag_name}, token => $token);
            ## Ignore the token
            $token = $self->_get_next_token;
          } else {
            
            pop @{$self->{open_elements}};
            $token = $self->_get_next_token;
          }

          if (not defined $self->{inner_html_node} and
              not ($self->{open_elements}->[-1]->[1] == FRAMESET_EL)) {
            
            $self->{insertion_mode} = AFTER_FRAMESET_IM;
          } else {
            
          }
          next B;
        } elsif ($token->{tag_name} eq 'html' and
                 $self->{insertion_mode} == AFTER_FRAMESET_IM) {
          
          $self->{insertion_mode} = AFTER_HTML_FRAMESET_IM;
          $token = $self->_get_next_token;
          next B;
        } else {
          if ($self->{insertion_mode} == IN_FRAMESET_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in frameset:/',
                            text => $token->{tag_name}, token => $token);
          } elsif ($self->{insertion_mode} == AFTER_FRAMESET_IM) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after frameset:/',
                            text => $token->{tag_name}, token => $token);
          } else { # "after after html"
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'after after frameset:/',
                            text => $token->{tag_name}, token => $token);
          }
          ## Ignore the token
          $token = $self->_get_next_token;
          next B;
        }
      } elsif ($token->{type} == END_OF_FILE_TOKEN) {
        unless ($self->{open_elements}->[-1]->[1] == HTML_EL and
                @{$self->{open_elements}} == 1) { # redundant, maybe
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in body:#eof', token => $token);
        } else {
          
        }
        
        ## Stop parsing
        last B;
      } else {
        die "$0: $token->{type}: Unknown token type";
      }
    } else {
      die "$0: $self->{insertion_mode}: Unknown insertion mode";
    }

    ## "in body" insertion mode
    if ($token->{type} == START_TAG_TOKEN) {
      if ($token->{tag_name} eq 'script') {
        
        ## NOTE: This is an "as if in head" code clone
        $script_start_tag->($self, $insert, $open_tables);
        next B;
      } elsif ($token->{tag_name} eq 'style') {
        
        ## NOTE: This is an "as if in head" code clone
        $parse_rcdata->($self, $insert, $open_tables, 0); # RAWTEXT
        next B;
      } elsif ({
                base => 1, command => 1, link => 1,
               }->{$token->{tag_name}}) {
        
        ## NOTE: This is an "as if in head" code clone, only "-t" differs
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        pop @{$self->{open_elements}};
        delete $self->{self_closing};
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'meta') {
        ## NOTE: This is an "as if in head" code clone, only "-t" differs
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        my $meta_el = pop @{$self->{open_elements}};

        unless ($self->{confident}) {
          if ($token->{attributes}->{charset}) {
            
            ## NOTE: Whether the encoding is supported or not is handled
            ## in the {change_encoding} callback.
            $self->{change_encoding}
                ->($self, $token->{attributes}->{charset}->{value}, $token);
            
            DATA($meta_el->[0]->getAttributeNodeNS(undef, 'charset'),
                manakai_has_reference => $token->{attributes}->{charset}->{has_reference});
					 
          } elsif ($token->{attributes}->{content}) {
            if ($token->{attributes}->{content}->{value}
                =~ /[Cc][Hh][Aa][Rr][Ss][Ee][Tt]
                    [\x09\x0A\x0C\x0D\x20]*=
                    [\x09\x0A\x0C\x0D\x20]*(?>"([^"]*)"|'([^']*)'|
                    ([^"'\x09\x0A\x0C\x0D\x20][^\x09\x0A\x0C\x0D\x20\x3B]*))
                   /x) {
              
              ## NOTE: Whether the encoding is supported or not is handled
              ## in the {change_encoding} callback.
              $self->{change_encoding}
                  ->($self, defined $1 ? $1 : defined $2 ? $2 : $3, $token);
              DATA($meta_el->[0]->getAttributeNodeNS(undef, 'content'),
                  manakai_has_reference => $token->{attributes}->{content}->{has_reference});
            }
          }
        } else {
          if ($token->{attributes}->{charset}) {
            
            DATA($meta_el->[0]->getAttributeNodeNS(undef, 'charset'),
                manakai_has_reference => $token->{attributes}->{charset}->{has_reference});
          }
          if ($token->{attributes}->{content}) {
            
            DATA($meta_el->[0]->getAttributeNodeNS (undef, 'content'),
					manakai_has_reference => $token->{attributes}->{content}->{has_reference});
          }
        }

        delete $self->{self_closing};
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'title') {
        
        ## NOTE: This is an "as if in head" code clone
        $parse_rcdata->($self, $insert, $open_tables, 1); # RCDATA
        next B;
      } elsif ($token->{tag_name} eq 'body') {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'in body', text => 'body', token => $token);
              
        if (@{$self->{open_elements}} == 1 or
            not ($self->{open_elements}->[1]->[1] == BODY_EL)) {
          
          ## Ignore the token
        } else {
          my $body_el = $self->{open_elements}->[1]->[0];
          for my $attr_name (keys %{$token->{attributes}}) {
            unless ($body_el->hasAttributeNS(undef, $attr_name)) {
              
              $body_el->setAttributeNS(undef, $attr_name,
                 $token->{attributes}->{$attr_name}->{value});
            }
          }
        }
        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'frameset') {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'in body', text => $token->{tag_name},
                        token => $token);

        if (@{$self->{open_elements}} == 1 or
            not ($self->{open_elements}->[1]->[1] == BODY_EL)) {
          
          ## Ignore the token.
        } elsif (not $self->{frameset_ok}) {
          
          ## Ignore the token.
        } else {
          
          
          ## 1. Remove the second element.
          my $body = $self->{open_elements}->[1]->[0];
          my $body_parent = $body->parentNode;
          $body_parent->removeChild ($body) if $body_parent;

          ## 2. Pop nodes.
          splice @{$self->{open_elements}}, 1;

          ## 3. Insert.
          
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  

          ## 4. Switch.
          $self->{insertion_mode} = IN_FRAMESET_IM;
        }

        
        $token = $self->_get_next_token;
        next B;
      } elsif ({
                ## NOTE: Start tags for non-phrasing flow content elements

                ## NOTE: The normal one
                address => 1, article => 1, aside => 1, blockquote => 1,
                center => 1, 
                #datagrid => 1,
                details => 1, 
                dir => 1, div => 1, dl => 1, fieldset => 1, figure => 1,
                footer => 1, h1 => 1, h2 => 1, h3 => 1, h4 => 1, h5 => 1,
                h6 => 1, header => 1, hgroup => 1,
                menu => 1, nav => 1, ol => 1, p => 1, 
                section => 1, ul => 1,
                ## NOTE: As normal, but drops leading newline
                pre => 1, listing => 1,
                ## NOTE: As normal, but interacts with the form element pointer
                form => 1,
                
                table => 1,
                hr => 1,
               }->{$token->{tag_name}}) {

        ## 1. When there is an opening |form| element:
        if ($token->{tag_name} eq 'form' and defined $self->{form_element}) {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'in form:form', token => $token);
          ## Ignore the token
          
          $token = $self->_get_next_token;
          next B;
        }

        ## 2. Close the |p| element, if any.
        if ($token->{tag_name} ne 'table' or # The Hixie Quirk
            (DATA($self->{document})->{'manakai_compat_mode'}||'') ne 'quirks') {
          ## "has a |p| element in scope"
          INSCOPE: for (reverse @{$self->{open_elements}}) {
            if ($_->[1] == P_EL) {
              
              
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <form>
              $token = {type => END_TAG_TOKEN, tag_name => 'p',
                        line => $token->{line}, column => $token->{column}};
              next B;
            } elsif ($_->[1] & SCOPING_EL) {
              
              last INSCOPE;
            }
          } # INSCOPE
        }

        ## 3. Close the opening <hn> element, if any.
        if ({h1 => 1, h2 => 1, h3 => 1,
             h4 => 1, h5 => 1, h6 => 1}->{$token->{tag_name}}) {
          if ($self->{open_elements}->[-1]->[1] == HEADING_EL) {
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                            text => $self->{open_elements}->[-1]->[0]->tagName,
                            token => $token);
            pop @{$self->{open_elements}};
          }
        }

        ## 4. Insertion.
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        if ($token->{tag_name} eq 'pre' or $token->{tag_name} eq 'listing') {
          
          $token = $self->_get_next_token;
          if ($token->{type} == CHARACTER_TOKEN) {
            $token->{data} =~ s/^\x0A//;
            unless (length $token->{data}) {
              
              $token = $self->_get_next_token;
            } else {
              
            }
          } else {
            
          }

          delete $self->{frameset_ok};
        } elsif ($token->{tag_name} eq 'form') {
          
          $self->{form_element} = $self->{open_elements}->[-1]->[0];

          
          $token = $self->_get_next_token;
        } elsif ($token->{tag_name} eq 'table') {
          
          push @{$open_tables}, [$self->{open_elements}->[-1]->[0]];

          delete $self->{frameset_ok};
          
          $self->{insertion_mode} = IN_TABLE_IM;

          
          $token = $self->_get_next_token;
        } elsif ($token->{tag_name} eq 'hr') {
          
          pop @{$self->{open_elements}};
          
          delete $self->{self_closing};

          delete $self->{frameset_ok};

          $token = $self->_get_next_token;
        } else {
          
          $token = $self->_get_next_token;
        }
        next B;
      } elsif ($token->{tag_name} eq 'li') {
        ## NOTE: As normal, but imply </li> when there's another <li> ...

        ## NOTE: Special, Scope (<li><foo><li> == <li><foo><li/></foo></li>)::
          ## Interpreted as <li><foo/></li><li/> (non-conforming):
          ## blockquote (O9.27), center (O), dd (Fx3, O, S3.1.2, IE7),
          ## dt (Fx, O, S, IE), dl (O), fieldset (O, S, IE), form (Fx, O, S),
          ## hn (O), pre (O), applet (O, S), button (O, S), marquee (Fx, O, S),
          ## object (Fx)
          ## Generate non-tree (non-conforming):
          ## basefont (IE7 (where basefont is non-void)), center (IE),
          ## form (IE), hn (IE)
        ## address, div, p (<li><foo><li> == <li><foo/></li><li/>)::
          ## Interpreted as <li><foo><li/></foo></li> (non-conforming):
          ## div (Fx, S)

        ## 1. Frameset-ng
        delete $self->{frameset_ok};

        my $non_optional;
        my $i = -1;

        ## 2.
        for my $node (reverse @{$self->{open_elements}}) {
          if ($node->[1] == LI_EL) {
            ## 3. (a) As if </li>
            {
              ## If no </li> - not applied
              #

              ## Otherwise

              ## 1. generate implied end tags, except for </li>
              #

              ## 2. If current node != "li", parse error
              if ($non_optional) {
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                                text => $non_optional->[0]->tagName,
                                token => $token);
                
              } else {
                
              }

              ## 3. Pop
              splice @{$self->{open_elements}}, $i;
            }

            last; ## 3. (b) goto 5.
          } elsif (
                   ## NOTE: not "formatting" and not "phrasing"
                   ($node->[1] & SPECIAL_EL or
                    $node->[1] & SCOPING_EL) and
                   ## NOTE: "li", "dt", and "dd" are in |SPECIAL_EL|.
                   (not $node->[1] & ADDRESS_DIV_P_EL)
                  ) {
            ## 4.
            
            last; ## goto 6.
          } elsif ($node->[1] & END_TAG_OPTIONAL_EL) {
            
            #
          } else {
            
            $non_optional ||= $node;
            #
          }
          ## 5.
          ## goto 3.
          $i--;
        }

        ## 6. (a) has a |p| element in scope
        INSCOPE: for (reverse @{$self->{open_elements}}) {
          if ($_->[1] == P_EL) {
            

            ## NOTE: |<p><li>|, for example.

            
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <x>
            $token = {type => END_TAG_TOKEN, tag_name => 'p',
                      line => $token->{line}, column => $token->{column}};
            next B;
          } elsif ($_->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE

        ## 6. (b) insert
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'dt' or
               $token->{tag_name} eq 'dd') {
        ## NOTE: As normal, but imply </dt> or </dd> when ...

        ## 1. Frameset-ng
        delete $self->{frameset_ok};

        my $non_optional;
        my $i = -1;

        ## 2.
        for my $node (reverse @{$self->{open_elements}}) {
          if ($node->[1] == DTDD_EL) {
            ## 3. (a) As if </li>
            {
              ## If no </li> - not applied
              #

              ## Otherwise

              ## 1. generate implied end tags, except for </dt> or </dd>
              #

              ## 2. If current node != "dt"|"dd", parse error
              if ($non_optional) {
                $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                                text => $non_optional->[0]->tagName,
                                token => $token);
                
              } else {
                
              }

              ## 3. Pop
              splice @{$self->{open_elements}}, $i;
            }

            last; ## 3. (b) goto 5.
          } elsif (
                   ## NOTE: not "formatting" and not "phrasing"
                   ($node->[1] & SPECIAL_EL or
                    $node->[1] & SCOPING_EL) and
                   ## NOTE: "li", "dt", and "dd" are in |SPECIAL_EL|.

                   (not $node->[1] & ADDRESS_DIV_P_EL)
                  ) {
            ## 4.
            
            last; ## goto 5.
          } elsif ($node->[1] & END_TAG_OPTIONAL_EL) {
            
            #
          } else {
            
            $non_optional ||= $node;
            #
          }
          ## 5.
          ## goto 3.
          $i--;
        }

        ## 6. (a) has a |p| element in scope
        INSCOPE: for (reverse @{$self->{open_elements}}) {
          if ($_->[1] == P_EL) {
            
            
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <x>
            $token = {type => END_TAG_TOKEN, tag_name => 'p',
                      line => $token->{line}, column => $token->{column}};
            next B;
          } elsif ($_->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE

        ## 6. (b) insert
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'plaintext') {
        ## NOTE: As normal, but effectively ends parsing

        ## has a p element in scope
        INSCOPE: for (reverse @{$self->{open_elements}}) {
          if ($_->[1] == P_EL) {
            
            
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <plaintext>
            $token = {type => END_TAG_TOKEN, tag_name => 'p',
                      line => $token->{line}, column => $token->{column}};
            next B;
          } elsif ($_->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE
          
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        $self->{state} = PLAINTEXT_STATE;
          
        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'a') {
        AFE: for my $i (reverse 0..$#$active_formatting_elements) {
          my $node = $active_formatting_elements->[$i];
			 no warnings;
          if ($node->[1] == A_EL) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in a:a', token => $token);
            
            
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <a>
            $token = {type => END_TAG_TOKEN, tag_name => 'a',
                      line => $token->{line}, column => $token->{column}};
            $formatting_end_tag->($self, $active_formatting_elements,
                                  $open_tables, $token);
            
            AFE2: for (reverse 0..$#$active_formatting_elements) {
              if ($active_formatting_elements->[$_]->[0] eq $node->[0]) {
                
                splice @$active_formatting_elements, $_, 1;
                last AFE2;
              }
            } # AFE2
            OE: for (reverse 0..$#{$self->{open_elements}}) {
              if ($self->{open_elements}->[$_]->[0] eq $node->[0]) {
                
                splice @{$self->{open_elements}}, $_, 1;
                last OE;
              }
            } # OE
            last AFE;
          } elsif ($node->[0] eq '#marker') {
            
            last AFE;
          }
        } # AFE
          
        $reconstruct_active_formatting_elements
          ->($self, $insert_to_current, $active_formatting_elements, $open_tables);

        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          if ($attr)
          {
            $attr->setValue($attr_t->{value});
            DATA($attr, manakai_source_line => $attr_t->{line});
            DATA($attr, manakai_source_column => $attr_t->{column});
            $el->setAttributeNodeNS ($attr);
          }
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        push @$active_formatting_elements, 
          [$self->{open_elements}->[-1]->[0],
          $self->{open_elements}->[-1]->[1],
          $token];

        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'nobr') {
        $reconstruct_active_formatting_elements
          ->($self, $insert_to_current, $active_formatting_elements, $open_tables);

        ## has a |nobr| element in scope
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] == NOBR_EL) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in nobr:nobr', token => $token);
            
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <nobr>
            $token = {type => END_TAG_TOKEN, tag_name => 'nobr',
                      line => $token->{line}, column => $token->{column}};
            next B;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE
        
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        push @$active_formatting_elements, 
          [$self->{open_elements}->[-1]->[0],
          $self->{open_elements}->[-1]->[1],
          $token];        
        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'button') {
        ## has a button element in scope
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] == BUTTON_EL) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'in button:button', token => $token);
            
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <button>
            $token = {type => END_TAG_TOKEN, tag_name => 'button',
                      line => $token->{line}, column => $token->{column}};
            next B;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE
          
        $reconstruct_active_formatting_elements
          ->($self, $insert_to_current, $active_formatting_elements, $open_tables);
          
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  

        ## TODO: associate with $self->{form_element} if defined

        push @$active_formatting_elements, ['#marker', '', undef];

        delete $self->{frameset_ok};

        
        $token = $self->_get_next_token;
        next B;
      } elsif ({
                xmp => 1,
                iframe => 1,
                noembed => 1,
                noframes => 1, ## NOTE: This is an "as if in head" code clone.
                noscript => 0, ## TODO: 1 if scripting is enabled
               }->{$token->{tag_name}}) {
        if ($token->{tag_name} eq 'xmp') {
			  
          ## "has a |p| element in scope".
          INSCOPE: for (reverse @{$self->{open_elements}}) {
            if ($_->[1] == P_EL) {
              
              
        $token->{self_closing} = $self->{self_closing};
        unshift @{$self->{token}}, $token;
        delete $self->{self_closing};
       # <xmp>
              $token = {type => END_TAG_TOKEN, tag_name => 'p',
                        line => $token->{line}, column => $token->{column}};
              next B;
            } elsif ($_->[1] & SCOPING_EL) {
              
              last INSCOPE;
            }
          } # INSCOPE
          
          $reconstruct_active_formatting_elements
            ->($self, $insert_to_current, $active_formatting_elements, $open_tables);

          delete $self->{frameset_ok};
        } elsif ($token->{tag_name} eq 'iframe') {
          
          delete $self->{frameset_ok};
        } else {
          
        }
        ## NOTE: There is an "as if in body" code clone.
        $parse_rcdata->($self, $insert, $open_tables, 0); # RAWTEXT
        next B;
      } elsif ($token->{tag_name} eq 'isindex') {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'isindex', token => $token);
        
        if (defined $self->{form_element}) {
          
          ## Ignore the token
           ## NOTE: Not acknowledged.
          $token = $self->_get_next_token;
          next B;
        } else {
          delete $self->{self_closing};

          my $at = $token->{attributes};
          my $form_attrs;
          $form_attrs->{action} = $at->{action} if $at->{action};
          my $prompt_attr = $at->{prompt};
          $at->{name} = {name => 'name', value => 'isindex'};
          delete $at->{action};
          delete $at->{prompt};
          my @tokens = (
                        {type => START_TAG_TOKEN, tag_name => 'form',
                         attributes => $form_attrs,
                         line => $token->{line}, column => $token->{column}},
                        {type => START_TAG_TOKEN, tag_name => 'hr',
                         line => $token->{line}, column => $token->{column}},
                        {type => START_TAG_TOKEN, tag_name => 'label',
                         line => $token->{line}, column => $token->{column}},
                       );
          if ($prompt_attr) {
            
            push @tokens, {type => CHARACTER_TOKEN, data => $prompt_attr->{value},
                           #line => $token->{line}, column => $token->{column},
                          };
          } else {
            
            push @tokens, {type => CHARACTER_TOKEN,
                           data => 'This is a searchable index. Insert your search keywords here: ',
                           #line => $token->{line}, column => $token->{column},
                          }; # SHOULD
            ## TODO: make this configurable
          }
          push @tokens,
                        {type => START_TAG_TOKEN, tag_name => 'input', attributes => $at,
                         line => $token->{line}, column => $token->{column}},
                        #{type => CHARACTER_TOKEN, data => ''}, # SHOULD
                        {type => END_TAG_TOKEN, tag_name => 'label',
                         line => $token->{line}, column => $token->{column}},
                        {type => START_TAG_TOKEN, tag_name => 'hr',
                         line => $token->{line}, column => $token->{column}},
                        {type => END_TAG_TOKEN, tag_name => 'form',
                         line => $token->{line}, column => $token->{column}};
          unshift @{$self->{token}}, (@tokens);
          $token = $self->_get_next_token;
          next B;
        }
      } elsif ($token->{tag_name} eq 'textarea') {
        ## 1. Insert
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        
        ## Step 2 # XXX
        ## TODO: $self->{form_element} if defined

        ## 2. Drop U+000A LINE FEED
        $self->{ignore_newline} = 1;

        ## 3. RCDATA
        $self->{state} = RCDATA_STATE;
        delete $self->{escape}; # MUST

        ## 4., 6. Insertion mode
        $self->{insertion_mode} |= IN_CDATA_RCDATA_IM;

        ## 5. Frameset-ng.
        delete $self->{frameset_ok};

        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'optgroup' or
               $token->{tag_name} eq 'option') {
        ## has an |option| element in scope
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] == OPTION_EL) {
            
            ## NOTE: As if </option>
            
      $token->{self_closing} = $self->{self_closing};
      unshift @{$self->{token}}, $token;
      delete $self->{self_closing};
     # <option> or <optgroup>
            $token = {type => END_TAG_TOKEN, tag_name => 'option',
                      line => $token->{line}, column => $token->{column}};
            next B;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE

        $reconstruct_active_formatting_elements
          ->($self, $insert_to_current, $active_formatting_elements, $open_tables);

        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  

        
        $token = $self->_get_next_token;
        redo B;
      } elsif ($token->{tag_name} eq 'rt' or
               $token->{tag_name} eq 'rp') {
        ## has a |ruby| element in scope
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] == RUBY_EL) {
            
            ## generate implied end tags
            while ($self->{open_elements}->[-1]->[1] & END_TAG_OPTIONAL_EL) {
              
              pop @{$self->{open_elements}};
            }
            unless ($self->{open_elements}->[-1]->[1] == RUBY_EL) {
              
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                              text => $self->{open_elements}->[-1]->[0]
                                  ->tagName,
                              token => $token);
              pop @{$self->{open_elements}}
                  while not $self->{open_elements}->[-1]->[1] == RUBY_EL;
            }
            last INSCOPE;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE
          
        ## TODO: <non-ruby><rt> is not allowed.

        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  

        
        $token = $self->_get_next_token;
        redo B;
      } elsif ($token->{tag_name} eq 'math' or
               $token->{tag_name} eq 'svg') {
        $reconstruct_active_formatting_elements
          ->($self, $insert_to_current, $active_formatting_elements, $open_tables);

        ## "Adjust MathML attributes" ('math' only) - done in insert-element-f

        ## "adjust SVG attributes" ('svg' only) - done in insert-element-f

        ## "adjust foreign attributes" - done in insert-element-f
        
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS
			($token->{tag_name} eq 'math' ? (MML_NS) : (SVG_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr;
			 if (defined $foreign_attr_xname->{ $attr_name })
			 {
				 my $xmlnsuri = $foreign_attr_xname->{ $attr_name }->[0];
				 my $qname = join ':', @{$foreign_attr_xname->{ $attr_name }->[1]};
				 $qname =~ s/(^:)|(:$)//;
				 $attr = $self->{document}->createAttributeNS($xmlnsuri, $qname);
			 }
			 elsif ($token->{tag_name} eq 'math' && $attr_name eq 'definitionurl')
			 {
				 $attr = $self->{document}->createAttributeNS((MML_NS), 'definitionURL');
			 }
			 elsif ($token->{tag_name} eq 'math')
			 {
				 $attr = $self->{document}->createAttributeNS((MML_NS), $attr_name);
			 }
			 elsif ($token->{tag_name} eq 'svg')
			 {
				 $attr = $self->{document}->createAttributeNS(
					(SVG_NS), ($svg_attr_name->{$attr_name} || $attr_name));
			 }
			 unless ($attr)
			 {
				$attr = $self->{document}->createAttribute($attr_name);
				}
			if ($attr)
			{
				 $attr->setValue($attr_t->{value});
				 DATA($attr, manakai_source_line => $attr_t->{line});
				 DATA($attr, manakai_source_column => $attr_t->{column});
				 $el->setAttributeNodeNS ($attr);
			 }
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, ($el_category_f->{$token->{tag_name} eq 'math' ? (MML_NS) : (SVG_NS)}->{ $token->{tag_name}} || 0) | FOREIGN_EL];

      if ( $token->{attributes}->{xmlns} and  $token->{attributes}->{xmlns}->{value} ne ($token->{tag_name} eq 'math' ? (MML_NS) : (SVG_NS))) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad namespace', token =>  $token);
## TODO: Error type documentation
      }
      if ( $token->{attributes}->{'xmlns:xlink'} and
           $token->{attributes}->{'xmlns:xlink'}->{value} ne q<http://www.w3.org/1999/xlink>) {
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'bad namespace', token =>  $token);
      }
    }
  
        
        if ($self->{self_closing}) {
          pop @{$self->{open_elements}};
          delete $self->{self_closing};
        } else {
          
          $self->{insertion_mode} |= IN_FOREIGN_CONTENT_IM;
          ## NOTE: |<body><math><mi><svg>| -> "in foreign content" insertion
          ## mode, "in body" (not "in foreign content") secondary insertion
          ## mode, maybe.
        }

        $token = $self->_get_next_token;
        next B;
      } elsif ({
                caption => 1, col => 1, colgroup => 1, frame => 1,
                head => 1,
                tbody => 1, td => 1, tfoot => 1, th => 1,
                thead => 1, tr => 1,
               }->{$token->{tag_name}}) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'in body',
                        text => $token->{tag_name}, token => $token);
        ## Ignore the token
         ## NOTE: |<col/>| or |<frame/>| here is an error.
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'param' or
               $token->{tag_name} eq 'source') {
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS(undef, $attr_name);
          $attr->setValue($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  
        pop @{$self->{open_elements}};

        delete $self->{self_closing};
        $token = $self->_get_next_token;
        redo B;
      } else {
        if ($token->{tag_name} eq 'image') {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'image', token => $token);
          $token->{tag_name} = 'img';
        } else {
          
        }

        ## NOTE: There is an "as if <br>" code clone.
        $reconstruct_active_formatting_elements
          ->($self, $insert_to_current, $active_formatting_elements, $open_tables);
        
        
    {
      my $el;
      
      $el = $self->{document}->createElementNS((HTML_NS), $token->{tag_name});
    
        for my $attr_name (keys %{  $token->{attributes}}) {
          my $attr_t =   $token->{attributes}->{$attr_name};
          my $attr = $self->{document}->createAttributeNS (undef, $attr_name);
          $attr->setValue ($attr_t->{value});
          DATA($attr, manakai_source_line => $attr_t->{line});
          DATA($attr, manakai_source_column => $attr_t->{column});
          $el->setAttributeNodeNS ($attr);
        }
      
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
      $insert->($self, $el, $open_tables);
      push @{$self->{open_elements}}, [$el, $el_category->{$token->{tag_name}} || 0];
    }
  

        if ({
             applet => 1, marquee => 1, object => 1,
            }->{$token->{tag_name}}) {
          

          push @$active_formatting_elements, ['#marker', '', undef];

          delete $self->{frameset_ok};

          
        } elsif ({
                  b => 1, big => 1, code=>1, em => 1, font => 1, i => 1,
                  s => 1, small => 1, strike => 1,
                  strong => 1, tt => 1, u => 1,
                 }->{$token->{tag_name}}) {
          
        push @$active_formatting_elements, 
          [$self->{open_elements}->[-1]->[0],
          $self->{open_elements}->[-1]->[1],
          $token];
          
        } elsif ($token->{tag_name} eq 'input') {
          
          ## TODO: associate with $self->{form_element} if defined
          pop @{$self->{open_elements}};
          delete $self->{self_closing};
        } elsif ({
                  area => 1, basefont => 1, bgsound => 1, br => 1,
                  embed => 1, img => 1, wbr => 1,
                  keygen => 1,
                 }->{$token->{tag_name}}) {
          

          pop @{$self->{open_elements}};

          delete $self->{frameset_ok};

          delete $self->{self_closing};
        } elsif ($token->{tag_name} eq 'select') {
          ## TODO: associate with $self->{form_element} if defined

          delete $self->{frameset_ok};
          
          if ($self->{insertion_mode} & TABLE_IMS or
              $self->{insertion_mode} & BODY_TABLE_IMS or
              ($self->{insertion_mode} & IM_MASK) == IN_COLUMN_GROUP_IM) {
            
            $self->{insertion_mode} = IN_SELECT_IN_TABLE_IM;
          } else {
            
            $self->{insertion_mode} = IN_SELECT_IM;
          }
          
        } else {
          
        }
        
        $token = $self->_get_next_token;
        next B;
      }
    } elsif ($token->{type} == END_TAG_TOKEN) {
      if ($token->{tag_name} eq 'body' or $token->{tag_name} eq 'html') {

        ## 1. If not "have an element in scope":
        ## "has a |body| element in scope"
        my $i;
        INSCOPE: {
          for (reverse @{$self->{open_elements}}) {
            if ($_->[1] == BODY_EL) {
              
              $i = $_;
              last INSCOPE;
            } elsif ($_->[1] & SCOPING_EL) {
              
              last;
            }
          }

          ## NOTE: |<marquee></body>|, |<svg><foreignobject></body>|,
          ## and fragment cases.

          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);
          ## Ignore the token.  (</body> or </html>)
          $token = $self->_get_next_token;
          next B;
        } # INSCOPE

        ## 2. If unclosed elements:
        for (@{$self->{open_elements}}) {
          unless ($_->[1] & ALL_END_TAG_OPTIONAL_EL ||
                  $_->[1] == OPTGROUP_EL ||
                  $_->[1] == OPTION_EL ||
                  $_->[1] == RUBY_COMPONENT_EL) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                            text => $_->[0]->tagName,
                            token => $token);
            last;
          } else {
            
          }
        }

        ## 3. Switch the insertion mode.
        $self->{insertion_mode} = AFTER_BODY_IM;
        if ($token->{tag_name} eq 'body') {
          $token = $self->_get_next_token;
        } else { # html
          ## Reprocess.
        }
        next B;
      } elsif ({
                ## NOTE: End tags for non-phrasing flow content elements

                ## NOTE: The normal ones
                address => 1, article => 1, aside => 1, blockquote => 1,
                center => 1, 
                #datagrid => 1, 
                details => 1, 
                dir => 1, div => 1, dl => 1, fieldset => 1, figure => 1,
                footer => 1, header => 1, hgroup => 1,
                listing => 1, menu => 1, nav => 1,
                ol => 1, pre => 1, section => 1, ul => 1,

                ## NOTE: As normal, but ... optional tags
                dd => 1, dt => 1, li => 1,

                applet => 1, button => 1, marquee => 1, object => 1,
               }->{$token->{tag_name}}) {
        ## NOTE: Code for <li> start tags includes "as if </li>" code.
        ## Code for <dt> or <dd> start tags includes "as if </dt> or
        ## </dd>" code.

        ## has an element in scope
        my $i;
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[0]->tagName eq $token->{tag_name}) {
            
            $i = $_;
            last INSCOPE;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
          elsif ($token->{tag_name} eq 'li' and
                   {ul => 1, ol => 1}->{$node->[0]->localname}) {
            ## Has an element in list item scope
            
            last INSCOPE;
           }
        } # INSCOPE

        unless (defined $i) { # has an element in scope
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);
          ## NOTE: Ignore the token.
        } else {
          ## Step 1. generate implied end tags
          while ({
                  ## END_TAG_OPTIONAL_EL
                  dd => ($token->{tag_name} ne 'dd'),
                  dt => ($token->{tag_name} ne 'dt'),
                  li => ($token->{tag_name} ne 'li'),
                  option => 1,
                  optgroup => 1,
                  p => 1,
                  rt => 1,
                  rp => 1,
                 }->{$self->{open_elements}->[-1]->[0]->tagName}) {
            
            pop @{$self->{open_elements}};
          }

          ## Step 2.
          if ($self->{open_elements}->[-1]->[0]->tagName
                  ne $token->{tag_name}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                            text => $self->{open_elements}->[-1]->[0]
                                ->tagName,
                            token => $token);
          } else {
            
          }

          ## Step 3.
          splice @{$self->{open_elements}}, $i;

          ## Step 4.
          $clear_up_to_marker->($active_formatting_elements)
              if {
                applet => 1, button => 1, marquee => 1, object => 1,
              }->{$token->{tag_name}};
        }
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'form') {
        ## NOTE: As normal, but interacts with the form element pointer

        undef $self->{form_element};

        ## has an element in scope
        my $i;
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] == FORM_EL) {
            
            $i = $_;
            last INSCOPE;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE

        unless (defined $i) { # has an element in scope
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);
          ## NOTE: Ignore the token.
        } else {
          ## Step 1. generate implied end tags
          while ($self->{open_elements}->[-1]->[1] & END_TAG_OPTIONAL_EL) {
            
            pop @{$self->{open_elements}};
          }
          
          ## Step 2. 
          if ($self->{open_elements}->[-1]->[0]->tagName
                  ne $token->{tag_name}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                            text => $self->{open_elements}->[-1]->[0]
                                ->tagName,
                            token => $token);
          } else {
            
          }  
          
          ## Step 3.
          splice @{$self->{open_elements}}, $i;
        }

        $token = $self->_get_next_token;
        next B;
      } elsif ({
                ## NOTE: As normal, except acts as a closer for any ...
                h1 => 1, h2 => 1, h3 => 1, h4 => 1, h5 => 1, h6 => 1,
               }->{$token->{tag_name}}) {
        ## has an element in scope
        my $i;
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] == HEADING_EL) {
            
            $i = $_;
            last INSCOPE;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          }
        } # INSCOPE

        unless (defined $i) { # has an element in scope
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);
          ## NOTE: Ignore the token.
        } else {
          ## Step 1. generate implied end tags
          while ($self->{open_elements}->[-1]->[1] & END_TAG_OPTIONAL_EL) {
            
            pop @{$self->{open_elements}};
          }
          
          ## Step 2.
          if ($self->{open_elements}->[-1]->[0]->tagName
                  ne $token->{tag_name}) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                            text => $token->{tag_name}, token => $token);
          } else {
            
          }

          ## Step 3.
          splice @{$self->{open_elements}}, $i;
        }
        
        $token = $self->_get_next_token;
        next B;
      } elsif ($token->{tag_name} eq 'p') {
        ## NOTE: As normal, except </p> implies <p> and ...

        ## has an element in scope
        my $non_optional;
        my $i;
        INSCOPE: for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] == P_EL) {
            
            $i = $_;
            last INSCOPE;
          } elsif ($node->[1] & SCOPING_EL) {
            
            last INSCOPE;
          } elsif ($node->[1] & END_TAG_OPTIONAL_EL) {
            ## NOTE: |END_TAG_OPTIONAL_EL| includes "p"
            
            #
          } else {
            
            $non_optional ||= $node;
            #
          }
        } # INSCOPE

        if (defined $i) {
          ## 1. Generate implied end tags
          #

          ## 2. If current node != "p", parse error
          if ($non_optional) {
            
            $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                            text => $non_optional->[0]->tagName,
                            token => $token);
          } else {
            
          }

          ## 3. Pop
          splice @{$self->{open_elements}}, $i;
        } else {
          
          $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                          text => $token->{tag_name}, token => $token);

          
          ## As if <p>, then reprocess the current token
          my $el;
          
      $el = $self->{document}->createElementNS((HTML_NS), 'p');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
          $insert->($self, $el, $open_tables);
          ## NOTE: Not inserted into |$self->{open_elements}|.
        }

        $token = $self->_get_next_token;
        next B;
      } elsif ({
                a => 1,
                b => 1, big => 1, code=>1, em => 1, font => 1, i => 1,
                nobr => 1, s => 1, small => 1, strike => 1,
                strong => 1, tt => 1, u => 1,
               }->{$token->{tag_name}}) {
        
        $formatting_end_tag->($self, $active_formatting_elements,
                              $open_tables, $token);
        next B;
      } elsif ($token->{tag_name} eq 'br') {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                        text => 'br', token => $token);

        ## As if <br>
        $reconstruct_active_formatting_elements
          ->($self, $insert_to_current, $active_formatting_elements, $open_tables);
        
        my $el;
        
      $el = $self->{document}->createElementNS((HTML_NS), 'br');
    
        DATA($el, manakai_source_line => $token->{line})
            if defined $token->{line};
        DATA($el, manakai_source_column => $token->{column})
            if defined $token->{column};
      
        $insert->($self, $el, $open_tables);
        
        ## Ignore the token.
        $token = $self->_get_next_token;
        next B;
      } else {
        if ($token->{tag_name} eq 'sarcasm') {
          sleep 0.001; # take a deep breath
        }

        ## Step 1
        my $node_i = -1;
        my $node = $self->{open_elements}->[$node_i];

        ## Step 2
        S2: {
          my $node_tag_name = $node->[0]->tagName;
          $node_tag_name =~ tr/A-Z/a-z/; # for SVG camelCase tag names
          if ($node_tag_name eq $token->{tag_name}) {
            ## Step 1
            ## generate implied end tags
            while ($self->{open_elements}->[-1]->[1] & END_TAG_OPTIONAL_EL) {
              
              ## NOTE: |<ruby><rt></ruby>|.
              ## ISSUE: <ruby><rt></rt> will also take this code path,
              ## which seems wrong.
              pop @{$self->{open_elements}};
              $node_i++;
            }
        
            ## Step 2
            my $current_tag_name
                = $self->{open_elements}->[-1]->[0]->tagName;
            $current_tag_name =~ tr/A-Z/a-z/;
            if ($current_tag_name ne $token->{tag_name}) {
              
              ## NOTE: <x><y></x>
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'not closed',
                              text => $self->{open_elements}->[-1]->[0]
                                  ->tagName,
                              token => $token);
            } else {
              
            }
            
            ## Step 3
            splice @{$self->{open_elements}}, $node_i if $node_i < 0;

            $token = $self->_get_next_token;
            last S2;
          } else {
            ## Step 3
            if (not ($node->[1] & FORMATTING_EL) and
                #not $phrasing_category->{$node->[1]} and
                ($node->[1] & SPECIAL_EL or
                 $node->[1] & SCOPING_EL)) {
              
              $self->{parse_error}->(level => $self->{level}->{must}, type => 'unmatched end tag',
                              text => $token->{tag_name}, token => $token);
              ## Ignore the token
              $token = $self->_get_next_token;
              last S2;

              ## NOTE: |<span><dd></span>a|: In Safari 3.1.2 and Opera
              ## 9.27, "a" is a child of <dd> (conforming).  In
              ## Firefox 3.0.2, "a" is a child of <body>.  In WinIE 7,
              ## "a" is a child of both <body> and <dd>.
            }
            
            
          }
          
          ## Step 4
          $node_i--;
          $node = $self->{open_elements}->[$node_i];
          
          ## Step 5;
          redo S2;
        } # S2
	next B;
      }
    }
    next B;
  } continue { # B
    if ($self->{insertion_mode} & IN_FOREIGN_CONTENT_IM) {
      ## NOTE: The code below is executed in cases where it does not have
      ## to be, but it it is harmless even in those cases.
      ## has an element in scope
      INSCOPE: {
        for (reverse 0..$#{$self->{open_elements}}) {
          my $node = $self->{open_elements}->[$_];
          if ($node->[1] & FOREIGN_EL) {
            last INSCOPE;
          } elsif ($node->[1] & SCOPING_EL) {
            last;
          }
        }
        
        ## NOTE: No foreign element in scope.
        $self->{insertion_mode} &= ~ IN_FOREIGN_CONTENT_IM;
      } # INSCOPE
    }
  } # B

  ## Stop parsing # MUST
  
  ## TODO: script stuffs
} # _tree_construct_main

## XXX: How this method is organized is somewhat out of date, although
## it still does what the current spec documents.
sub set_inner_html ($$$$;$) {
  my $class = shift;
  my $node = shift; # /context/
  #my $s = \$_[0];
  my $onerror = $_[1];
  my $get_wrapper = $_[2] || sub ($) { return $_[0] };

  my $nt = $node->node_type; #TOBY-TODO
  if ($nt == 9) { # Document (invoke the algorithm with no /context/ element)
    # MUST
    
    ## Step 1 # MUST
    ## TODO: If the document has an active parser, ...
    ## ISSUE: There is an issue in the spec.
    
    ## Step 2 # MUST
    my @cn = $node->childNodes;
    for (@cn) {
      $node->removeChild ($_);
    }

    ## Step 3, 4, 5 # MUST
    $class->parse_char_string ($_[0] => $node, $onerror, $get_wrapper);
  } elsif ($nt == 1) { # Element (invoke the algorithm with /context/ element)
    ## TODO: If non-html element

    ## NOTE: Most of this code is copied from |parse_string|

## TODO: Support for $get_wrapper
#TOBY-TODO
    ## F1. Create an HTML document.
    my $this_doc = $node->ownerDocument;
	 my $implementation = ref($this_doc);
	 my $doc = $implementation->createDocument;
    DATA($doc, manakai_is_html => 1);

    ## F2. Propagate quirkness flag
    my $node_doc = $node->ownerDocument;
    DATA($doc)->{'manakai_compat_mode'} = DATA($node_doc, 'manakai_compat_mode');

    ## F3. Create an HTML parser
    my $p = $class->new;
    $p->{document} = $doc;

    ## Step 8 # MUST
    my $i = 0;
    $p->{line_prev} = $p->{line} = 1;
    $p->{column_prev} = $p->{column} = 0;
    require HTML::HTML5::Parser::Charset::DecodeHandle;
    my $input = HTML::HTML5::Parser::Charset::DecodeHandle::CharString->new (\($_[0]));
    $input = $get_wrapper->($input);
    $p->{set_nc} = sub {
      my $self = shift;

      my $char = '';
      if (defined $self->{next_nc}) {
        $char = $self->{next_nc};
        delete $self->{next_nc};
        $self->{nc} = ord $char;
      } else {
        $self->{char_buffer} = '';
        $self->{char_buffer_pos} = 0;
        
        my $count = $input->manakai_read_until
            ($self->{char_buffer}, qr/[^\x00\x0A\x0D\x{D800}-\x{DFFF}]/,
             $self->{char_buffer_pos});
        if ($count) {
          $self->{line_prev} = $self->{line};
          $self->{column_prev} = $self->{column};
          $self->{column}++;
          $self->{nc}
              = ord substr ($self->{char_buffer},
                            $self->{char_buffer_pos}++, 1);
          return;
        }
        
        if ($input->read ($char, 1)) {
          $self->{nc} = ord $char;
        } else {
          $self->{nc} = -1;
          return;
        }
      }

      ($p->{line_prev}, $p->{column_prev}) = ($p->{line}, $p->{column});
      $p->{column}++;

      if ($self->{nc} == 0x000A) { # LF
        $p->{line}++;
        $p->{column} = 0;
        
      } elsif ($self->{nc} == 0x000D) { # CR
## TODO: support for abort/streaming
        my $next = '';
        if ($input->read ($next, 1) and $next ne "\x0A") {
          $self->{next_nc} = $next;
        }
        $self->{nc} = 0x000A; # LF # MUST
        $p->{line}++;
        $p->{column} = 0;
        
      } elsif ($self->{nc} == 0x0000) { # NULL
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'NULL');
        $self->{nc} = 0xFFFD; # REPLACEMENT CHARACTER # MUST
      } elsif (0xD800 <= $self->{nc} and $self->{nc} <= 0xDFFF) {
        
        $self->{parse_error}->(level => $self->{level}->{must}, type => 'surrogate'); ## XXX documentation
        $self->{nc} = 0xFFFD; # REPLACEMENT CHARACTER # MUST
       }
    };

    $p->{read_until} = sub {
      #my ($scalar, $specials_range, $offset) = @_;
      return 0 if defined $p->{next_nc};

      my $pattern = qr/[^$_[1]\x00\x0A\x0D\x{D800}-\x{DFFF}]/;
      my $offset = $_[2] || 0;
      
      if ($p->{char_buffer_pos} < length $p->{char_buffer}) {
        pos ($p->{char_buffer}) = $p->{char_buffer_pos};
        if ($p->{char_buffer} =~ /\G(?>$pattern)+/) {
          substr ($_[0], $offset)
              = substr ($p->{char_buffer}, $-[0], $+[0] - $-[0]);
          my $count = $+[0] - $-[0];
          if ($count) {
            $p->{column} += $count;
            $p->{char_buffer_pos} += $count;
            $p->{line_prev} = $p->{line};
            $p->{column_prev} = $p->{column} - 1;
            $p->{nc} = -1;
          }
          return $count;
        } else {
          return 0;
        }
      } else {
        my $count = $input->manakai_read_until ($_[0], $pattern, $_[2]);
        if ($count) {
          $p->{column} += $count;
          $p->{column_prev} += $count;
          $p->{nc} = -1;
        }
        return $count;
      }
    }; # $p->{read_until}

    my $ponerror = $onerror || sub {
      my (%opt) = @_;
      my $line = $opt{line};
      my $column = $opt{column};
      if (defined $opt{token} and defined $opt{token}->{line}) {
        $line = $opt{token}->{line};
        $column = $opt{token}->{column};
      }
      warn "Parse error ($opt{type}) at line $line column $column\n";
    };
    $p->{parse_error} = sub {
      $ponerror->(line => $p->{line}, column => $p->{column}, @_);
    };
    
    my $char_onerror = sub {
      my (undef, $type, %opt) = @_;
      $ponerror->(layer => 'encode',
                  line => $p->{line}, column => $p->{column} + 1,
                  %opt, type => $type);
    }; # $char_onerror
    $input->onerror ($char_onerror);

    $p->_initialize_tokenizer;
    $p->_initialize_tree_constructor;

    ## F4. If /context/ is not undef...

    ## F4.1. content model flag
    my $node_ln = $node->tagName;
  if ($node_ln eq 'title' or $node_ln eq 'textarea') {
    $p->{state} = RCDATA_STATE;
  } elsif ($node_ln eq 'script') {
    $p->{state} = SCRIPT_DATA_STATE;
  } elsif ({
      style => 1,
      script => 1,
      xmp => 1,
      iframe => 1,
      noembed => 1,
      noframes => 1,
      noscript => 1,
    }->{$node_ln}) {
    $p->{state} = RAWTEXT_STATE;
  } elsif ($node_ln eq 'plaintext') {
    $p->{state} = PLAINTEXT_STATE;
  }
  
    $p->{inner_html_node} = [$node, $el_category->{$node_ln}];
      ## TODO: Foreign element OK?

    ## F4.2. Root |html| element
    my $root = $doc->createElementNS('http://www.w3.org/1999/xhtml', 'html');

    ## F4.3.
    $doc->appendChild ($root);

    ## F4.4.
    push @{$p->{open_elements}}, [$root, $el_category->{html}];

    undef $p->{head_element};
    undef $p->{head_element_inserted};

    ## F4.5.
    $p->_reset_insertion_mode;

    ## F4.6.
    my $anode = $node;
    AN: while (defined $anode) {
      if ($anode->node_type == 1) {
        my $nsuri = $anode->namespaceURI;
        if (defined $nsuri and $nsuri eq 'http://www.w3.org/1999/xhtml') {
          if ($anode->tagName eq 'form') {
            
            $p->{form_element} = $anode;
            last AN;
          }
        }
      }
      $anode = $anode->parentNode;
    } # AN

    ## F.5. Set the input stream.
    $p->{confident} = 1; ## Confident: irrelevant.

    ## F.6. Start the parser.
    {
      my $self = $p;
      $token = $self->_get_next_token;
    }
    $p->_tree_construction_main;

    ## F.7.
    my @cn = $node->childNodes;
    for (@cn) {
      $node->removeChild ($_);
    }
    ## ISSUE: mutation events? read-only?

    ## Step 11 # MUST
    @cn = $root->childNodes;
    for (@cn) {
      $this_doc->adoptNode ($_);
      $node->appendChild ($_);
    }
    ## ISSUE: mutation events?

    $p->_terminate_tree_constructor;

    ## Remove self references. 
    delete $p->{set_nc}; 
    delete $p->{read_until}; 
    delete $p->{parse_error}; 
  } else {
    die "$0: |set_inner_html| is not defined for node of type $nt";
  }
} # set_inner_html

} # tree construction stage

package HTML::HTML5::Parser::TagSoupParser::RestartParser;
push our @ISA, 'Error';

1;
# $Date: 2009/09/06 23:32:06 $
