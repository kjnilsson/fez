-module('System.Char').
-export([
		'IsControl'/1,
		'IsControl'/2,
		'IsDigit'/1,
		'IsDigit'/2,
		'IsLetter'/1,
		'IsLetter'/2,
		'IsLetterOrDigit'/1,
		'IsLetterOrDigit'/2,
		'IsLowSurrogate'/1,
		'IsLowSurrogate'/2,
		'IsHighSurrogate'/1,
		'IsHighSurrogate'/2,
		'IsLower'/1,
		'IsLower'/2,
		'IsUpper'/1,
		'IsUpper'/2,
		'IsNumber'/1,
		'IsNumber'/2,
		'IsPunctuation'/1,
		'IsPunctuation'/2,
		'IsSeparator'/1,
		'IsSeparator'/2,
		'IsWhiteSpace'/1,
		'IsWhiteSpace'/2,
		'Parse'/1,
		'ToLower'/1,
		'ToUpper'/1,
		'TryParse'/1
	]).


%% TODO: add unicode specific methods

%% erlang encoding can be of the following types
% endian() = big | little
% encoding() =
%     latin1 |
%     unicode |
%     utf8 |
%     utf16 |
%     {utf16, endian()} |
%     utf32 |
%     {utf32, endian()}


%% -------------------------------------- Internal --------------------------------------

%% Helper
%% If the index is valid, call the function with the values, Fun must return bool
%% for_valid_index: (char -> bool) -> str -> int -> bool
for_valid_index(Fun, Str, Idx) ->
	(Idx >= 0) and (Idx < string:len(Str)) and Fun(lists:nth(Idx, Str)).

%% Title case letters, such as
%% --   U+01C5 (LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON)
%%   to U+1FFC (GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI)
%% These characters are members of the UnicodeCategory.TitlecaseLetter category.
'IsTitleCase'(Chr) ->
	(Chr >= 16#01C5) and (Chr =< 16#1FFC).


%% Modifiers, such as 
%% --      U+02B0 (MODIFIER LETTER SMALL H)
%% through U+02C1 (MODIFIER LETTER REVERSED GLOTTAL STOP)
%%         U+1D2C (MODIFIER LETTER CAPITAL A)
%% through U+1D61 (MODIFIER LETTER SMALL CHI).
%% These characters are members of the UnicodeCategory.ModifierLetter category.
'IsModifierLetter'(Chr) ->
	((Chr >= 16#02B0) and (Chr =< 16#02C1)) or ((Chr >= 16#1D2C) and (Chr =< 16#1D61)).


%% Other letters, such
%%   as    U+05D0 (HEBREW LETTER ALEF)
%% through U+05EA (HEBREW LETTER TAV),
%%         U+0621 (ARABIC LETTER HAMZA)
%% through U+063A (ARABIC LETTER GHAIN)
%%         U+4E00 (<CJK Ideograph, First>)
%% through U+9FC3 (<CJK Ideograph, Last>).
%% These characters are members of the UnicodeCategory.OtherLetter category.
'IsOtherLetter'(Chr) ->
	((Chr >= 16#05D0) and (Chr =< 16#05EA)) or 
	((Chr >= 16#0621) and (Chr =< 16#063A)) or 
	((Chr >= 16#4E00) and (Chr =< 16#9FC3)).
%% -------------------------------------- End Internal --------------------------------------


%% Indicates whether the specified Unicode character is categorized as a control character.
%% IsControl : char → bool
%% must be between [\U0000 .. \U001F], \U007F, [\U0080 .. \U009F]
'IsControl'(Chr) ->
	((Chr >= 16#0000) and (Chr =< 16#001F)) or 
	(Chr == 16#007F) or 
	((Chr >= 16#0080) and (Chr =< 16#009F)).
'IsControl'(Str, Idx) ->
	for_valid_index(fun 'IsControl'/1, Str, Idx).


%% Indicates whether the character is categorized as a decimal digit.
%% IsDigit(Char)
% TODO: unicode? are there unicode digits? infinity? constants?(pi,tau etc.)
'IsDigit'(Chr) ->
	(Chr >= 48) and (Chr =< 57).
'IsDigit'(Str, Idx) ->
	for_valid_index(fun 'IsDigit'/1, Str, Idx).


%% Indicates whether the character is categorized as a lowercase letter.
%% IsLower: Char -> bool
%% Lowercase letters, such as
%% -- U+0061 (LATIN SMALL LETTER A) through U+007A (LATIN SMALL LETTER Z)
%% -- U+03AC (GREEK SMALL LETTER ALPHA WITH TONOS) through U+03CE (GREEK SMALL LETTER OMEGA WITH TONOS)
'IsLower'(Chr) ->
	((Chr >= 16#0061) and (Chr =< 16#007A)) or 
	((Chr >= 16#03AC) and (Chr =< 16#03CE)).
'IsLower'(Str, Idx) ->
	for_valid_index(fun 'IsLower'/1, Str, Idx).

% Indicates whether the Char object at the specified position in a string is a low surrogate.
%% IsLowSurrogate(Char)	
%% between 16#DC00..16#DFFF.
%% TODO, convert to utf-16 and check?
'IsLowSurrogate'(Chr) ->
	(Chr >= 16#DC00) and (Chr =< 16#DFFF).
'IsLowSurrogate'(Str,Idx) ->
	for_valid_index(fun 'IsLowSurrogate'/1, Str, Idx).


% Indicates whether the character is categorized as an uppercase letter.
% IsUpper(Char)	
%% TODO, add a lot more
%% Uppercase letters, such as 
%% -- U+0041 (LATIN CAPITAL LETTER A) through U+005A (LATIN CAPITAL LETTER Z)
%% -- U+0400 (CYRILLIC CAPITAL LETTER IE WITH GRAVE) through U+042F (CYRILLIC CAPITAL LETTER YA)
'IsUpper'(Chr) ->
	((Chr >= 16#0041) and (Chr =< 16#005A)) or 
	((Chr >= 16#0400) and (Chr =< 16#042F)).
'IsUpper'(Str, Idx) ->
	for_valid_index(fun 'IsUpper'/1, Str, Idx).

%% Indicates whether the Char object at the specified position in a string is a high surrogate.
%% IsHighSurrogate: Char -> bool
%% between 16#D800..16#DBFF?
%% TODO, convert to utf-16 and check? Might need to pattern match over two chars
'IsHighSurrogate'(Chr) ->
	(Chr >= 16#D800) and (Chr =< 16#DBFF).
'IsHighSurrogate'(Str,Idx) ->
	for_valid_index(fun 'IsHighSurrogate'/1, Str, Idx).


%% Indicates whether the character is categorized as a Unicode letter.
%% IsLetter: Char -> bool
'IsLetter'(Chr) ->
	'IsUpper'(Chr) or 
	'IsLower'(Chr) or 
	'IsTitleCase'(Chr) or 
	'IsModifierLetter'(Chr) or 
	'IsOtherLetter'(Chr).
'IsLetter'(Str, Idx) ->
	for_valid_index(fun 'IsLetter'/1, Str, Idx).


%% Indicates whether the character is categorized as a letter or a decimal digit.
% IsLetterOrDigit: Char -> bool
'IsLetterOrDigit'(Chr) ->
	'IsDigit'(Chr) or 'IsLetter'(Chr).
'IsLetterOrDigit'(Str,Idx) ->
	for_valid_index(fun 'IsLetterOrDigit'/1, Str, Idx).


% Indicates whether the character is categorized as a number.
% IsNumber(Char)
% TODO, figure out what this means and implement for unicode
'IsNumber'(Chr) ->
	'IsDigit'(Chr).
'IsNumber'(Str,Idx) ->
	for_valid_index(fun 'IsNumber'/1, Str, Idx).

%% phew! should generate the below -- unused, left here as documentation
%% "!\"#،؍᠀᠁᠂᠃᠄᠅᠆᠇᠈᠉᠊〔〕〖〗〘〙〚〛〜〝〞〟%&'()*؛᥄᥅〰,-./؞؟᧞᧟〽:;٪٫٬٭᨞᨟゠?@۔᭚᭛᭜᭝᭞᭟᭠・[\\]܀܁܂܃܄܅܆܇܈܉܊܋܌܍‐‑‒–—―‖‗‘’‚‛“”„‟†‡•‣․‥…‧꡴꡵꡶꡷_߷߸߹‰‱′″‴‵‶‷‸‹›※‼‽‾‿⁀⁁⁂⁃﴾﴿{।॥⁅⁆⁇⁈⁉⁊⁋⁌⁍⁎⁏⁐⁑︐︑︒︓︔︕︖︗︘︙}॰⁓⁔⁕⁖⁗⁘⁙⁚⁛⁜⁝⁞︰︱︲︳︴︵︶︷︸︹︺︻︼︽︾︿﹀﹁﹂﹃﹄﹅﹆﹇﹈﹉﹊﹋﹌﹍﹎﹏﹐﹑﹒¡෴⁽⁾﹔﹕﹖﹗﹘﹙﹚﹛﹜﹝﹞﹟﹠﹡«๏๐๑๒๓๔๕๖๗๘๙๚๛₍₎﹣­༄༅༆༇༈༉༊་༌།༎༏༐༑༒〈〉﹨·༺༻༼༽❨❩❪❫❬❭❮❯❰❱❲❳❴❵﹪﹫»྅⟅⟆！＂＃¿࿐࿑⟦⟧⟨⟩⟪⟫％＆＇（）＊;၊။၌၍၎၏⦃⦄⦅⦆⦇⦈⦉⦊⦋⦌⦍⦎⦏⦐⦑⦒⦓⦔⦕⦖⦗⦘，－．／·჻⧘⧙⧚⧛：；՚՛՜՝՞՟፡።፣፤፥፦፧፨⧼⧽？＠։֊᙭᙮⳹⳺⳻⳼［＼］־᚛᚜⳾⳿＿׀᛫᛬᛭⸀⸁⸂⸃⸄⸅⸆⸇⸈⸉⸊⸋⸌⸍⸎⸏⸐⸑⸒⸓⸔⸕⸖⸗｛׃᜵᜶⸜⸝｝׆។៕៖、。〃｟｠｡｢｣､･׳״៘៙៚〈〉《》「」『』【】"
% punctuationCharacterList() ->
% 	lists:append([
% 		lists:seq(16#0021, 16#0023), [16#060C, 16#060D], lists:seq(16#1800, 16#180A),
% 		lists:seq(16#3014, 16#301F), lists:seq(16#0025, 16#002A), [16#061B, 16#1944, 16#1945, 16#3030],
% 		lists:seq(16#002C, 16#002F), [16#061E, 16#061F, 16#19DE, 16#19DF, 16#303D, 16#003A, 16#003B],
% 		lists:seq(16#066A, 16#066D), [16#1A1E, 16#1A1F, 16#30A0, 16#003F, 16#0040, 16#06D4],
% 		lists:seq(16#1B5A, 16#1B60), [16#30FB], lists:seq(16#005B, 16#005D), lists:seq(16#0700, 16#070D),
% 		lists:seq(16#2010, 16#2027), lists:seq(16#A874, 16#A877), [16#005F], lists:seq(16#07F7, 16#07F9),
% 		lists:seq(16#2030, 16#2043), [16#FD3E, 16#FD3F, 16#007B, 16#0964, 16#0965], lists:seq(16#2045, 16#2051),
% 		lists:seq(16#FE10, 16#FE19), [16#007D, 16#0970], lists:seq(16#2053, 16#205E), lists:seq(16#FE30, 16#FE52),
% 		[16#00A1, 16#0DF4, 16#207D, 16#207E], lists:seq(16#FE54, 16#FE61), [16#00AB], lists:seq(16#0E4F, 16#0E5B),
% 		[16#208D, 16#208E, 16#FE63, 16#00AD], lists:seq(16#0F04, 16#0F12), [16#2329, 16#232A, 16#FE68, 16#00B7],
% 		lists:seq(16#0F3A, 16#0F3D), lists:seq(16#2768, 16#2775), [16#FE6A, 16#FE6B, 16#00BB, 16#0F85],
% 		lists:seq(16#27C5, 16#27C6), lists:seq(16#FF01, 16#FF03), [16#00BF, 16#0FD0, 16#0FD1],
% 		lists:seq(16#27E6, 16#27EB), lists:seq(16#FF05, 16#FF0A), [16#037E], lists:seq(16#104A, 16#104F),
% 		lists:seq(16#2983, 16#2998), lists:seq(16#FF0C, 16#FF0F), [16#0387, 16#10FB],
% 		lists:seq(16#29D8, 16#29DB), [16#FF1A, 16#FF1B], lists:seq(16#055A, 16#055F),
% 		lists:seq(16#1361, 16#1368), [16#29FC, 16#29FD, 16#FF1F, 16#FF20],
% 		[16#0589, 16#058A, 16#166D, 16#166E], lists:seq(16#2CF9, 16#2CFC), lists:seq(16#FF3B, 16#FF3D),
% 		[16#05BE, 16#169B, 16#169C, 16#2CFE, 16#2CFF, 16#FF3F, 16#05C0], lists:seq(16#16EB, 16#16ED),
% 		lists:seq(16#2E00, 16#2E17), [16#FF5B, 16#05C3, 16#1735, 16#1736, 16#2E1C, 16#2E1D],
% 		[16#FF5D, 16#05C6], lists:seq(16#17D4, 16#17D6), lists:seq(16#3001, 16#3003),
% 		lists:seq(16#FF5F, 16#FF65), [16#05F3, 16#05F4], lists:seq(16#17D8, 16#17DA),
% 		lists:seq(16#3008, 16#3011)
% 	]).

%% Optimized precalculated map literal for punctuations
punctuationCharacterMap() ->
	#{1792 => [],8243 => [],10090 => [],6105 => [],10088 => [],65282 => [], 8286 => [],65075 => [],65119 => [],8279 => [],12308 => [],65380 => [], 65293 => [],65307 => [],65044 => [],65116 => [],39 => [],12539 => [],
	  65343 => [],11796 => [],11793 => [],11790 => [],11513 => [],8224 => [], 8218 => [],4048 => [],65082 => [],63 => [],34 => [],3857 => [],3849 => [], 5868 => [],8284 => [],65377 => [],65093 => [],3666 => [],6151 => [],
	  65043 => [],4961 => [],3853 => [],8275 => [],65072 => [],2405 => [], 10093 => [],65113 => [],11797 => [],4049 => [],6622 => [],1797 => [], 65042 => [],1801 => [],3898 => [],6468 => [],8256 => [],12310 => [],
	  1417 => [],8248 => [],6102 => [],6686 => [],10627 => [],6145 => [], 3669 => [],64 => [],3663 => [],12448 => [],65108 => [],8249 => [], 11777 => [],65373 => [],161 => [],8252 => [],10636 => [],8261 => [],
	  10638 => [],1563 => [],11794 => [],894 => [],7008 => [],1478 => [], 12298 => [],4170 => [],11795 => [],6154 => [],65103 => [],11519 => [], 6469 => [],8245 => [],8209 => [],3856 => [],11787 => [],3848 => [],
	  4965 => [],1418 => [],8262 => [],4963 => [],2416 => [],6106 => [], 1802 => [],8278 => [],1549 => [],8222 => [],12316 => [],12314 => [], 65074 => [],12336 => [],125 => [],43127 => [],43126 => [],8211 => [],
	  65046 => [],8259 => [],12315 => [],65120 => [],10181 => [],3664 => [], 6687 => [],191 => [],5941 => [],8273 => [],11518 => [],10092 => [], 10715 => [],44 => [],8334 => [],10640 => [],4964 => [],7006 => [],
	  65102 => [],65079 => [],8263 => [],10218 => [],12291 => [],65091 => [], 65285 => [],65100 => [],11516 => [],10094 => [],8281 => [],1475 => [], 1794 => [],1375 => [],2404 => [],4175 => [],3850 => [],8216 => [],
	  10099 => [],10644 => [],3572 => [],12290 => [],65130 => [],65041 => [], 10646 => [],7003 => [],65131 => [],1524 => [],5788 => [],10100 => [], 12304 => [],5742 => [],1644 => [],8255 => [],65040 => [],37 => [],
	  3901 => [],903 => [],35 => [],10628 => [],65088 => [],12305 => [], 65086 => [],65288 => [],7002 => [],8213 => [],10641 => [],1642 => [], 65281 => [],65048 => [],6100 => [],65376 => [],65121 => [],8269 => [],
	  8229 => [],8264 => [],4173 => [],6144 => [],10101 => [],65073 => [], 45 => [],8266 => [],6146 => [],10648 => [],1374 => [],1567 => [],6147 => [], 65081 => [],10216 => [],8231 => [],10712 => [],65371 => [],8247 => [],
	  1795 => [],8285 => [],3847 => [],10632 => [],65104 => [],6623 => [], 3674 => [],10713 => [],65077 => [],41 => [],8265 => [],3973 => [], 3851 => [],6148 => [],8246 => [],91 => [],8221 => [],65085 => [],8317 => [],
	  33 => [],12311 => [],8230 => [],42 => [],65084 => [],8272 => [],65101 => [], 3673 => [],5867 => [],65290 => [],10639 => [],65311 => [],11515 => [], 4171 => [],3854 => [],6104 => [],8257 => [],8268 => [],10098 => [],
	  2041 => [],65294 => [],65045 => [],5942 => [],1799 => [],1800 => [], 1566 => [],3667 => [],10095 => [],12302 => [],8250 => [],4174 => [], 1548 => [],11780 => [],123 => [],6149 => [],12313 => [],12319 => [],
	  5787 => [],10214 => [],64830 => [],8242 => [],8318 => [],11779 => [], 95 => [],11785 => [],11788 => [],65105 => [],10217 => [],3844 => [], 65306 => [],8333 => [],8276 => [],12312 => [],65295 => [],8282 => [],
	  65292 => [],65096 => [],8270 => [],65289 => [],10647 => [],10645 => [], 11783 => [],12309 => [],8253 => [],8277 => [],6101 => [],65286 => [], 1796 => [],65095 => [],8223 => [],11789 => [],6152 => [],10219 => [],
	  8227 => [],10642 => [],8219 => [],10182 => [],1793 => [],65381 => [], 65283 => [],2039 => [],2040 => [],65080 => [],43125 => [],65087 => [], 4966 => [],12349 => [],65090 => [],3672 => [],11798 => [],3668 => [],
	  65089 => [],65099 => [],12297 => [],3845 => [],12289 => [],10097 => [], 4172 => [],8267 => [],3671 => [],4962 => [],10749 => [],65339 => [], 10714 => [],3852 => [],64831 => [],1372 => [],173 => [],65078 => [],
	  10215 => [],12318 => [],9002 => [],8225 => [],11805 => [],8210 => [], 1470 => [],3665 => [],8240 => [],9001 => [],11781 => [],10096 => [], 10635 => [],43124 => [],47 => [],1523 => [],10748 => [],65379 => [],
	  3900 => [],8244 => [],10633 => [],40 => [],1472 => [],65375 => [],183 => [], 65128 => [],58 => [],187 => [],8215 => [],7005 => [],59 => [],65312 => [], 3855 => [],1748 => [],65378 => [],1804 => [],46 => [],11804 => [],171 => [],
	  3846 => [],65049 => [],8226 => [],11791 => [],65111 => [],4347 => [], 65340 => [],11782 => [],65106 => [],3675 => [],92 => [],3670 => [], 3899 => [],11784 => [],12300 => [],3858 => [],1371 => [],65109 => [],
	  65117 => [],1370 => [],8280 => [],11799 => [],65076 => [],8258 => [], 12303 => [],8271 => [],12317 => [],65118 => [],11786 => [],8217 => [], 1805 => [],8283 => [],93 => [],10089 => [],8214 => [],8241 => [],
	  12296 => [],4968 => [],10634 => [],8212 => [],11514 => [],65098 => [], 65341 => [],12301 => [],6153 => [],10643 => [],65094 => [],1373 => [],
	  12299 => [],11776 => [],65110 => [],65112 => [],65083 => [],65097 => [], 10631 => [],10637 => [],65287 => [],65092 => [],10091 => [],6150 => [], 1645 => [],10629 => [],65114 => [],65047 => [],8220 => [],38 => [],
	  1798 => [],10630 => [],5869 => [],65123 => [],8208 => [],8228 => [], 11792 => [],65115 => [],4967 => [],11778 => [],8254 => [],8251 => [], 7007 => [],1643 => [],7004 => [],1803 => [],5741 => []
  	}.
% Indicates whether the character is categorized as a punctuation mark.
% IsPunctuation(Char)
-compile({inline,[{punctuationCharacterMap,0}]}).
'IsPunctuation'(Chr) ->
	maps:is_key(Chr, punctuationCharacterMap()).
'IsPunctuation'(Str,Idx) ->
	for_valid_index(fun 'IsPunctuation'/1, Str, Idx).


spaceSeparators() -> 
	[16#0020, 16#00A0, 16#1680, 16#2000, 16#2001, 16#2002, 16#2003, 16#2004,
	 16#2005, 16#2006, 16#2007, 16#2008, 16#2009, 16#200A, 16#202F, 16#205F, 16#3000].
lineSeparator() -> 
	16#2028.
paragraphSeparator() ->
	16#2029.
% Indicates whether the character is categorized as a separator character.
% IsSeparator(Char)
'IsSeparator'(Chr) ->
	lists:member(Chr, lists:append(spaceSeparators(), [lineSeparator(), paragraphSeparator()])).
'IsSeparator'(Str, Idx) ->
	for_valid_index(fun 'IsSeparator'/1, Str, Idx).


% Indicates whether the character has a surrogate code unit.
% 'IsSurrogate'(Chr) ->
% 	'IsSeparator'(Chr).
% 'IsSurrogate'(Str,Idx) ->
% 	for_valid_index(fun 'IsSurrogate'/1, Str, Idx).
% TODO


% Indicates whether the two specified Char objects form a surrogate pair.
% IsSurrogatePair(Char, Char)	
% TODO


% Indicates whether two adjacent Char objects at a specified position in a string form a surrogate pair.
% IsSurrogatePair(String, Int32)	
% TODO


% Indicates whether the specified Unicode character is categorized as a symbol character.
% IsSymbol(Char)	
% TODO
% IsSymbol(String, Int32)	
% TODO

% Indicates whether the character is categorized as white space.
% IsWhiteSpace(Char)
% SPACE (U+0020), NO-BREAK SPACE (U+00A0), OGHAM SPACE MARK (U+1680), EN QUAD (U+2000), EM QUAD (U+2001), EN SPACE (U+2002), EM SPACE (U+2003), THREE-PER-EM SPACE (U+2004), FOUR-PER-EM SPACE (U+2005), SIX-PER-EM SPACE (U+2006), FIGURE SPACE (U+2007), PUNCTUATION SPACE (U+2008), THIN SPACE (U+2009), HAIR SPACE (U+200A), NARROW NO-BREAK SPACE (U+202F), MEDIUM MATHEMATICAL SPACE (U+205F), and IDEOGRAPHIC SPACE (U+3000).
% LINE SEPARATOR character (U+2028).
% PARAGRAPH SEPARATOR character (U+2029).
% CHARACTER TABULATION (U+0009), LINE FEED (U+000A), LINE TABULATION (U+000B), FORM FEED (U+000C), CARRIAGE RETURN (U+000D), and NEXT LINE (U+0085). 
whiteSpaceChars() ->
	[16#0020, 16#00A0, 16#1680, 16#2000, 16#2001, 16#2002, 16#2003, 16#2004, 16#2005,
	 16#2006, 16#2007, 16#2008, 16#2009, 16#200A, 16#202F, 16#205F, 16#3000,
	 16#2028, 16#2029, 16#0009, 16#000A, 16#000B, 16#000C, 16#000D, 16#0085
	].
'IsWhiteSpace'(Chr) ->
	lists:member(Chr, whiteSpaceChars()).
'IsWhiteSpace'(Str,Idx) ->
	for_valid_index(fun 'IsWhiteSpace'/1, Str, Idx).


% Converts the value of the specified string to its equivalent Unicode character.
% Parse: String -> Char
% TODO: not sure if right
'Parse'(Latin1Str) ->
	unicode:characters_to_list(list_to_binary(Latin1Str), utf8).


% Converts the value of a Unicode character to its lowercase equivalent.
% ToLower(Char)
'ToLower'(Chr) ->
	string:to_lower(Chr).

% Converts the value of a specified Unicode character to its lowercase equivalent using specified culture-specific formatting information.
% ToLower(Char, CultureInfo) ->
% TODO

% Converts the value of a Unicode character to its lowercase equivalent using the casing rules of the invariant culture.
% ToLowerInvariant(Char)
% TODO


% Converts the value of a Unicode character to its uppercase equivalent.
% ToUpper(Char)	
'ToUpper'(Chr) ->
	string:to_upper(Chr).

% Converts the value of a specified Unicode character to its uppercase equivalent using specified culture-specific formatting information.
% ToUpper(Char, CultureInfo)

% Converts the value of a Unicode character to its uppercase equivalent using the casing rules of the invariant culture.
% ToUpperInvariant(Char)


% Converts the value of this instance to its equivalent string representation.(Overrides ValueType.ToString().)
% ToString()	


% Converts the specified Unicode character to its equivalent string representation.
% ToString(Char)	


% Converts the value of this instance to its equivalent string representation using the specified culture-specific format information.
% ToString(IFormatProvider)	



% Converts the value of the specified string to its equivalent Unicode character. A return code indicates whether the conversion succeeded or failed.
% TryParse(String, Char) -> (bool, char)
% length of parsed string must be 1(the result should be one character).
%% TODO fix this
'TryParse'(Str) ->
	Res = 'Parse'(Str),
	case lists:len(Res) of
		1 -> {true, lists:nth(1, Res)};
		_ -> {false, 16#0000}
	end.
