(define *html-entities* 
  '(("quot" . "\u0022") ; (34)XML 1.0double quotation mark
    ("amp" . "\u0026") ; (38)XML 1.0ampersand
    ("apos" . "\u0027") ; (39)XML 1.0apostrophe (= apostrophe-quote)
    ("lt" . "\u003C") ; (60)XML 1.0less-than sign
    ("gt" . "\u003E") ; (62)XML 1.0greater-than sign
    ("quot" . "\u0022") ; (34)HTML 2.0HTMLspecialISOnumquotation mark (= APL quote)
    ("amp" . "\u0026") ; (38)HTML 2.0HTMLspecialISOnumampersand
    ("apos" . "\u0027") ; (39)XHTML 1.0HTMLspecialISOnumapostrophe (= apostrophe-quote); see below
    ("lt" . "\u003C") ; (60)HTML 2.0HTMLspecialISOnumless-than sign
    ("gt" . "\u003E") ; (62)HTML 2.0HTMLspecialISOnumgreater-than sign
    ("nbsp" . "\u00A0") ; (160)HTML 3.2HTMLlat1ISOnumno-break space (= non-breaking space)[d]
    ("iexcl" . "\u00A1") ; (161)HTML 3.2HTMLlat1ISOnuminverted exclamation mark
    ("cent" . "\u00A2") ; (162)HTML 3.2HTMLlat1ISOnumcent sign
    ("pound" . "\u00A3") ; (163)HTML 3.2HTMLlat1ISOnumpound sign
    ("curren" . "\u00A4") ; (164)HTML 3.2HTMLlat1ISOnumcurrency sign
    ("yen" . "\u00A5") ; (165)HTML 3.2HTMLlat1ISOnumyen sign (= yuan sign)
    ("brvbar" . "\u00A6") ; (166)HTML 3.2HTMLlat1ISOnumbroken bar (= broken vertical bar)
    ("sect" . "\u00A7") ; (167)HTML 3.2HTMLlat1ISOnumsection sign
    ("uml" . "\u00A8") ; (168)HTML 3.2HTMLlat1ISOdiadiaeresis (= spacing diaeresis); see Germanic umlaut
    ("copy" . "\u00A9") ; (169)HTML 3.2HTMLlat1ISOnumcopyright symbol
    ("ordf" . "\u00AA") ; (170)HTML 3.2HTMLlat1ISOnumfeminine ordinal indicator
    ("laquo" . "\u00AB") ; (171)HTML 3.2HTMLlat1ISOnumleft-pointing double angle quotation mark (= left pointing gu
    ("not" . "\u00AC") ; (172)HTML 3.2HTMLlat1ISOnumnot sign
    ("shy" . "\u00AD") ; (173)HTML 3.2HTMLlat1ISOnumsoft hyphen (= discretionary hyphen)
    ("reg" . "\u00AE") ; (174)HTML 3.2HTMLlat1ISOnumregistered sign ( = registered trademark symbol)
    ("macr" . "\u00AF") ; (175)HTML 3.2HTMLlat1ISOdiamacron (= spacing macron = overline = APL overbar)
    ("deg" . "\u00B0") ; (176)HTML 3.2HTMLlat1ISOnumdegree symbol
    ("plusmn" . "\u00B1") ; (177)HTML 3.2HTMLlat1ISOnumplus-minus sign (= plus-or-minus sign)
    ("sup2" . "\u00B2") ; (178)HTML 3.2HTMLlat1ISOnumsuperscript two (= superscript digit two = squared)
    ("sup3" . "\u00B3") ; (179)HTML 3.2HTMLlat1ISOnumsuperscript three (= superscript digit three = cubed)
    ("acute" . "\u00B4") ; (180)HTML 3.2HTMLlat1ISOdiaacute accent (= spacing acute)
    ("micro" . "\u00B5") ; (181)HTML 3.2HTMLlat1ISOnummicro sign
    ("para" . "\u00B6") ; (182)HTML 3.2HTMLlat1ISOnumpilcrow sign ( = paragraph sign)
    ("middot" . "\u00B7") ; (183)HTML 3.2HTMLlat1ISOnummiddle dot (= Georgian comma = Greek middle dot)
    ("cedil" . "\u00B8") ; (184)HTML 3.2HTMLlat1ISOdiacedilla (= spacing cedilla)
    ("sup1" . "\u00B9") ; (185)HTML 3.2HTMLlat1ISOnumsuperscript one (= superscript digit one)
    ("ordm" . "\u00BA") ; (186)HTML 3.2HTMLlat1ISOnummasculine ordinal indicator
    ("raquo" . "\u00BB") ; (187)HTML 3.2HTMLlat1ISOnumright-pointing double angle quotation mark (= right pointing 
    ("frac14" . "\u00BC") ; (188)HTML 3.2HTMLlat1ISOnumvulgar fraction one quarter (= fraction one quarter)
    ("frac12" . "\u00BD") ; (189)HTML 3.2HTMLlat1ISOnumvulgar fraction one half (= fraction one half)
    ("frac34" . "\u00BE") ; (190)HTML 3.2HTMLlat1ISOnumvulgar fraction three quarters (= fraction three quarters)
    ("iquest" . "\u00BF") ; (191)HTML 3.2HTMLlat1ISOnuminverted question mark (= turned question mark)
    ("Agrave" . "\u00C0") ; (192)HTML 2.0HTMLlat1ISOlat1Latin capital letter A with grave accent (= Latin capital l
    ("Aacute" . "\u00C1") ; (193)HTML 2.0HTMLlat1ISOlat1Latin capital letter A with acute accent
    ("Acirc" . "\u00C2") ; (194)HTML 2.0HTMLlat1ISOlat1Latin capital letter A with circumflex
    ("Atilde" . "\u00C3") ; (195)HTML 2.0HTMLlat1ISOlat1Latin capital letter A with tilde
    ("Auml" . "\u00C4") ; (196)HTML 2.0HTMLlat1ISOlat1Latin capital letter A with diaeresis
    ("AElig" . "\u00C6") ; (198)HTML 2.0HTMLlat1ISOlat1Latin capital letter AE (= Latin capital ligature AE)
    ("Ccedil" . "\u00C7") ; (199)HTML 2.0HTMLlat1ISOlat1Latin capital letter C with cedilla
    ("Egrave" . "\u00C8") ; (200)HTML 2.0HTMLlat1ISOlat1Latin capital letter E with grave accent
    ("Eacute" . "\u00C9") ; (201)HTML 2.0HTMLlat1ISOlat1Latin capital letter E with acute accent
    ("Ecirc" . "\u00CA") ; (202)HTML 2.0HTMLlat1ISOlat1Latin capital letter E with circumflex
    ("Euml" . "\u00CB") ; (203)HTML 2.0HTMLlat1ISOlat1Latin capital letter E with diaeresis
    ("Igrave" . "\u00CC") ; (204)HTML 2.0HTMLlat1ISOlat1Latin capital letter I with grave accent
    ("Iacute" . "\u00CD") ; (205)HTML 2.0HTMLlat1ISOlat1Latin capital letter I with acute accent
    ("Icirc" . "\u00CE") ; (206)HTML 2.0HTMLlat1ISOlat1Latin capital letter I with circumflex
    ("Iuml" . "\u00CF") ; (207)HTML 2.0HTMLlat1ISOlat1Latin capital letter I with diaeresis
    ("ETH" . "\u00D0") ; (208)HTML 2.0HTMLlat1ISOlat1Latin capital letter Eth
    ("Ntilde" . "\u00D1") ; (209)HTML 2.0HTMLlat1ISOlat1Latin capital letter N with tilde
    ("Ograve" . "\u00D2") ; (210)HTML 2.0HTMLlat1ISOlat1Latin capital letter O with grave accent
    ("Oacute" . "\u00D3") ; (211)HTML 2.0HTMLlat1ISOlat1Latin capital letter O with acute accent
    ("Ocirc" . "\u00D4") ; (212)HTML 2.0HTMLlat1ISOlat1Latin capital letter O with circumflex
    ("Otilde" . "\u00D5") ; (213)HTML 2.0HTMLlat1ISOlat1Latin capital letter O with tilde
    ("Ouml" . "\u00D6") ; (214)HTML 2.0HTMLlat1ISOlat1Latin capital letter O with diaeresis
    ("times" . "\u00D7") ; (215)HTML 3.2HTMLlat1ISOnummultiplication sign
    ("Oslash" . "\u00D8") ; (216)HTML 2.0HTMLlat1ISOlat1Latin capital letter O with stroke (= Latin capital letter ("Ugrave" . "\u00D9") ; (217)HTML 2.0HTMLlat1ISOlat1Latin capital letter U with grave accent
    ("Uacute" . "\u00DA") ; (218)HTML 2.0HTMLlat1ISOlat1Latin capital letter U with acute accent
    ("Ucirc" . "\u00DB") ; (219)HTML 2.0HTMLlat1ISOlat1Latin capital letter U with circumflex
    ("Uuml" . "\u00DC") ; (220)HTML 2.0HTMLlat1ISOlat1Latin capital letter U with diaeresis
    ("Yacute" . "\u00DD") ; (221)HTML 2.0HTMLlat1ISOlat1Latin capital letter Y with acute accent
    ("THORN" . "\u00DE") ; (222)HTML 2.0HTMLlat1ISOlat1Latin capital letter THORN
    ("szlig" . "\u00DF") ; (223)HTML 2.0HTMLlat1ISOlat1Latin small letter sharp s (= ess-zed); see German Eszett
    ("agrave" . "\u00E0") ; (224)HTML 2.0HTMLlat1ISOlat1Latin small letter a with grave accent
    ("aacute" . "\u00E1") ; (225)HTML 2.0HTMLlat1ISOlat1Latin small letter a with acute accent
    ("acirc" . "\u00E2") ; (226)HTML 2.0HTMLlat1ISOlat1Latin small letter a with circumflex
    ("atilde" . "\u00E3") ; (227)HTML 2.0HTMLlat1ISOlat1Latin small letter a with tilde
    ("auml" . "\u00E4") ; (228)HTML 2.0HTMLlat1ISOlat1Latin small letter a with diaeresis
    ("aring" . "\u00E5") ; (229)HTML 2.0HTMLlat1ISOlat1Latin small letter a with ring above
    ("aelig" . "\u00E6") ; (230)HTML 2.0HTMLlat1ISOlat1Latin small letter ae (= Latin small ligature ae)
    ("ccedil" . "\u00E7") ; (231)HTML 2.0HTMLlat1ISOlat1Latin small letter c with cedilla
    ("egrave" . "\u00E8") ; (232)HTML 2.0HTMLlat1ISOlat1Latin small letter e with grave accent
    ("eacute" . "\u00E9") ; (233)HTML 2.0HTMLlat1ISOlat1Latin small letter e with acute accent
    ("ecirc" . "\u00EA") ; (234)HTML 2.0HTMLlat1ISOlat1Latin small letter e with circumflex
    ("euml" . "\u00EB") ; (235)HTML 2.0HTMLlat1ISOlat1Latin small letter e with diaeresis
    ("igrave" . "\u00EC") ; (236)HTML 2.0HTMLlat1ISOlat1Latin small letter i with grave accent
    ("iacute" . "\u00ED") ; (237)HTML 2.0HTMLlat1ISOlat1Latin small letter i with acute accent
    ("icirc" . "\u00EE") ; (238)HTML 2.0HTMLlat1ISOlat1Latin small letter i with circumflex
    ("iuml" . "\u00EF") ; (239)HTML 2.0HTMLlat1ISOlat1Latin small letter i with diaeresis
    ("eth" . "\u00F0") ; (240)HTML 2.0HTMLlat1ISOlat1Latin small letter eth
    ("ntilde" . "\u00F1") ; (241)HTML 2.0HTMLlat1ISOlat1Latin small letter n with tilde
    ("ograve" . "\u00F2") ; (242)HTML 2.0HTMLlat1ISOlat1Latin small letter o with grave accent
    ("oacute" . "\u00F3") ; (243)HTML 2.0HTMLlat1ISOlat1Latin small letter o with acute accent
    ("ocirc" . "\u00F4") ; (244)HTML 2.0HTMLlat1ISOlat1Latin small letter o with circumflex
    ("otilde" . "\u00F5") ; (245)HTML 2.0HTMLlat1ISOlat1Latin small letter o with tilde
    ("ouml" . "\u00F6") ; (246)HTML 2.0HTMLlat1ISOlat1Latin small letter o with diaeresis
    ("divide" . "\u00F7") ; (247)HTML 3.2HTMLlat1ISOnumdivision sign (= obelus)
    ("oslash" . "\u00F8") ; (248)HTML 2.0HTMLlat1ISOlat1Latin small letter o with stroke (= Latin small letter o sl("ugrave" . "\u00F9") ; (249)HTML 2.0HTMLlat1ISOlat1Latin small letter u with grave accent
    ("uacute" . "\u00FA") ; (250)HTML 2.0HTMLlat1ISOlat1Latin small letter u with acute accent
    ("ucirc" . "\u00FB") ; (251)HTML 2.0HTMLlat1ISOlat1Latin small letter u with circumflex
    ("uuml" . "\u00FC") ; (252)HTML 2.0HTMLlat1ISOlat1Latin small letter u with diaeresis
    ("yacute" . "\u00FD") ; (253)HTML 2.0HTMLlat1ISOlat1Latin small letter y with acute accent
    ("thorn" . "\u00FE") ; (254)HTML 2.0HTMLlat1ISOlat1Latin small letter thorn
    ("yuml" . "\u00FF") ; (255)HTML 2.0HTMLlat1ISOlat1Latin small letter y with diaeresis
    ("OElig" . "\u0152") ; (338)HTML 4.0HTMLspecialISOlat2Latin capital ligature oe[e]
    ("oelig" . "\u0153") ; (339)HTML 4.0HTMLspecialISOlat2Latin small ligature oe[e]
    ("Scaron" . "\u0160") ; (352)HTML 4.0HTMLspecialISOlat2Latin capital letter s with caron
    ("scaron" . "\u0161") ; (353)HTML 4.0HTMLspecialISOlat2Latin small letter s with caron
    ("Yuml" . "\u0178") ; (376)HTML 4.0HTMLspecialISOlat2Latin capital letter y with diaeresis
    ("fnof" . "\u0192") ; (402)HTML 4.0HTMLsymbolISOtechLatin small letter f with hook (= function = florin)
    ("circ" . "\u02C6") ; (710)HTML 4.0HTMLspecialISOpubmodifier letter circumflex accent
    ("tilde" . "\u02DC") ; (732)HTML 4.0HTMLspecialISOdiasmall tilde
    ("Alpha" . "\u0391") ; (913)HTML 4.0HTMLsymbolGreek capital letter Alpha
    ("Beta" . "\u0392") ; (914)HTML 4.0HTMLsymbolGreek capital letter Beta
    ("Gamma" . "\u0393") ; (915)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Gamma
    ("Delta" . "\u0394") ; (916)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Delta
    ("Epsilon" . "\u0395") ; (917)HTML 4.0HTMLsymbolGreek capital letter Epsilon
    ("Zeta" . "\u0396") ; (918)HTML 4.0HTMLsymbolGreek capital letter Zeta
    ("Eta" . "\u0397") ; (919)HTML 4.0HTMLsymbolGreek capital letter Eta
    ("Theta" . "\u0398") ; (920)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Theta
    ("Iota" . "\u0399") ; (921)HTML 4.0HTMLsymbolGreek capital letter Iota
    ("Kappa" . "\u039A") ; (922)HTML 4.0HTMLsymbolGreek capital letter Kappa
    ("Lambda" . "\u039B") ; (923)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Lambda
    ("Mu" . "\u039C") ; (924)HTML 4.0HTMLsymbolGreek capital letter Mu
    ("Nu" . "\u039D") ; (925)HTML 4.0HTMLsymbolGreek capital letter Nu
    ("Xi" . "\u039E") ; (926)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Xi
    ("Omicron" . "\u039F") ; (927)HTML 4.0HTMLsymbolGreek capital letter Omicron
    ("Pi" . "\u03A0") ; (928)HTML 4.0HTMLsymbolGreek capital letter Pi
    ("Rho" . "\u03A1") ; (929)HTML 4.0HTMLsymbolGreek capital letter Rho
    ("Sigma" . "\u03A3") ; (931)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Sigma
    ("Tau" . "\u03A4") ; (932)HTML 4.0HTMLsymbolGreek capital letter Tau
    ("Upsilon" . "\u03A5") ; (933)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Upsilon
    ("Phi" . "\u03A6") ; (934)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Phi
    ("Chi" . "\u03A7") ; (935)HTML 4.0HTMLsymbolGreek capital letter Chi
    ("Psi" . "\u03A8") ; (936)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Psi
    ("Omega" . "\u03A9") ; (937)HTML 4.0HTMLsymbolISOgrk3Greek capital letter Omega
    ("alpha" . "\u03B1") ; (945)HTML 4.0HTMLsymbolISOgrk3Greek small letter alpha
    ("beta" . "\u03B2") ; (946)HTML 4.0HTMLsymbolISOgrk3Greek small letter beta
    ("gamma" . "\u03B3") ; (947)HTML 4.0HTMLsymbolISOgrk3Greek small letter gamma
    ("delta" . "\u03B4") ; (948)HTML 4.0HTMLsymbolISOgrk3Greek small letter delta
    ("epsilon" . "\u03B5") ; (949)HTML 4.0HTMLsymbolISOgrk3Greek small letter epsilon
    ("zeta" . "\u03B6") ; (950)HTML 4.0HTMLsymbolISOgrk3Greek small letter zeta
    ("eta" . "\u03B7") ; (951)HTML 4.0HTMLsymbolISOgrk3Greek small letter eta
    ("theta" . "\u03B8") ; (952)HTML 4.0HTMLsymbolISOgrk3Greek small letter theta
    ("iota" . "\u03B9") ; (953)HTML 4.0HTMLsymbolISOgrk3Greek small letter iota
    ("kappa" . "\u03BA") ; (954)HTML 4.0HTMLsymbolISOgrk3Greek small letter kappa
    ("lambda" . "\u03BB") ; (955)HTML 4.0HTMLsymbolISOgrk3Greek small letter lambda
    ("mu" . "\u03BC") ; (956)HTML 4.0HTMLsymbolISOgrk3Greek small letter mu
    ("nu" . "\u03BD") ; (957)HTML 4.0HTMLsymbolISOgrk3Greek small letter nu
    ("xi" . "\u03BE") ; (958)HTML 4.0HTMLsymbolISOgrk3Greek small letter xi
    ("omicron" . "\u03BF") ; (959)HTML 4.0HTMLsymbolNEWGreek small letter omicron
    ("pi" . "\u03C0") ; (960)HTML 4.0HTMLsymbolISOgrk3Greek small letter pi
    ("rho" . "\u03C1") ; (961)HTML 4.0HTMLsymbolISOgrk3Greek small letter rho
    ("sigmaf" . "\u03C2") ; (962)HTML 4.0HTMLsymbolISOgrk3Greek small letter final sigma
    ("sigma" . "\u03C3") ; (963)HTML 4.0HTMLsymbolISOgrk3Greek small letter sigma
    ("tau" . "\u03C4") ; (964)HTML 4.0HTMLsymbolISOgrk3Greek small letter tau
    ("upsilon" . "\u03C5") ; (965)HTML 4.0HTMLsymbolISOgrk3Greek small letter upsilon
    ("phi" . "\u03C6") ; (966)HTML 4.0HTMLsymbolISOgrk3Greek small letter phi
    ("chi" . "\u03C7") ; (967)HTML 4.0HTMLsymbolISOgrk3Greek small letter chi
    ("psi" . "\u03C8") ; (968)HTML 4.0HTMLsymbolISOgrk3Greek small letter psi
    ("omega" . "\u03C9") ; (969)HTML 4.0HTMLsymbolISOgrk3Greek small letter omega
    ("thetasym" . "\u03D1") ; (977)HTML 4.0HTMLsymbolNEWGreek theta symbol
    ("upsih" . "\u03D2") ; (978)HTML 4.0HTMLsymbolNEWGreek Upsilon with hook symbol
    ("piv" . "\u03D6") ; (982)HTML 4.0HTMLsymbolISOgrk3Greek pi symbol
    ("ensp" . "\u2002") ; (8194)HTML 4.0HTMLspecialISOpuben space[d]
    ("emsp" . "\u2003") ; (8195)HTML 4.0HTMLspecialISOpubem space[d]
    ("thinsp" . "\u2009") ; (8201)HTML 4.0HTMLspecialISOpubthin space[d]
    ("zwnj" . "\u200C") ; (8204)HTML 4.0HTMLspecialNEW RFC 2070zero-width non-joiner
    ("zwj" . "\u200D") ; (8205)HTML 4.0HTMLspecialNEW RFC 2070zero-width joiner
    ("lrm" . "\u200E") ; (8206)HTML 4.0HTMLspecialNEW RFC 2070left-to-right mark
    ("rlm" . "\u200F") ; (8207)HTML 4.0HTMLspecialNEW RFC 2070right-to-left mark
    ("ndash" . "\u2013") ; (8211)HTML 4.0HTMLspecialISOpuben dash
    ("mdash" . "\u2014") ; (8212)HTML 4.0HTMLspecialISOpubem dash
    ("lsquo" . "\u2018") ; (8216)HTML 4.0HTMLspecialISOnumleft single quotation mark
    ("rsquo" . "\u2019") ; (8217)HTML 4.0HTMLspecialISOnumright single quotation mark
    ("sbquo" . "\u201A") ; (8218)HTML 4.0HTMLspecialNEWsingle low-9 quotation mark
    ("ldquo" . "\u201C") ; (8220)HTML 4.0HTMLspecialISOnumleft double quotation mark
    ("rdquo" . "\u201D") ; (8221)HTML 4.0HTMLspecialISOnumright double quotation mark
    ("bdquo" . "\u201E") ; (8222)HTML 4.0HTMLspecialNEWdouble low-9 quotation mark
    ("dagger" . "\u2020") ; (8224)HTML 4.0HTMLspecialISOpubdagger, obelisk
    ("Dagger" . "\u2021") ; (8225)HTML 4.0HTMLspecialISOpubdouble dagger, double obelisk
    ("bull" . "\u2022") ; (8226)HTML 4.0HTMLspecialISOpubbullet (= black small circle)[f]
    ("hellip" . "\u2026") ; (8230)HTML 4.0HTMLsymbolISOpubhorizontal ellipsis (= three dot leader)
    ("permil" . "\u2030") ; (8240)HTML 4.0HTMLspecialISOtechper mille sign
    ("prime" . "\u2032") ; (8242)HTML 4.0HTMLsymbolISOtechprime (= minutes = feet)
    ("Prime" . "\u2033") ; (8243)HTML 4.0HTMLsymbolISOtechdouble prime (= seconds = inches)
    ("lsaquo" . "\u2039") ; (8249)HTML 4.0HTMLspecialISO proposedsingle left-pointing angle quotation mark[g]
    ("rsaquo" . "\u203A") ; (8250)HTML 4.0HTMLspecialISO proposedsingle right-pointing angle quotation mark[g]
    ("oline" . "\u203E") ; (8254)HTML 4.0HTMLsymbolNEWoverline (= spacing overscore)
    ("frasl" . "\u2044") ; (8260)HTML 4.0HTMLsymbolNEWfraction slash (= solidus)
    ("euro" . "\u20AC") ; (8364)HTML 4.0HTMLspecialNEWeuro sign
    ("image" . "\u2111") ; (8465)HTML 4.0HTMLsymbolISOamsoblack-letter capital I (= imaginary part)
    ("weierp" . "\u2118") ; (8472)HTML 4.0HTMLsymbolISOamsoscript capital P (= power set = Weierstrass p)
    ("real" . "\u211C") ; (8476)HTML 4.0HTMLsymbolISOamsoblack-letter capital R (= real part symbol)
    ("trade" . "\u2122") ; (8482)HTML 4.0HTMLsymbolISOnumtrademark symbol
    ("alefsym" . "\u2135") ; (8501)HTML 4.0HTMLsymbolNEWalef symbol (= first transfinite cardinal)[h]
    ("larr" . "\u2190") ; (8592)HTML 4.0HTMLsymbolISOnumleftwards arrow
    ("uarr" . "\u2191") ; (8593)HTML 4.0HTMLsymbolISOnumupwards arrow
    ("rarr" . "\u2192") ; (8594)HTML 4.0HTMLsymbolISOnumrightwards arrow
    ("darr" . "\u2193") ; (8595)HTML 4.0HTMLsymbolISOnumdownwards arrow
    ("harr" . "\u2194") ; (8596)HTML 4.0HTMLsymbolISOamsaleft right arrow
    ("crarr" . "\u21B5") ; (8629)HTML 4.0HTMLsymbolNEWdownwards arrow with corner leftwards (= carriage return)
    ("lArr" . "\u21D0") ; (8656)HTML 4.0HTMLsymbolISOtechleftwards double arrow[i]
    ("uArr" . "\u21D1") ; (8657)HTML 4.0HTMLsymbolISOamsaupwards double arrow
    ("rArr" . "\u21D2") ; (8658)HTML 4.0HTMLsymbolISOnumrightwards double arrow[j]
    ("dArr" . "\u21D3") ; (8659)HTML 4.0HTMLsymbolISOamsadownwards double arrow
    ("hArr" . "\u21D4") ; (8660)HTML 4.0HTMLsymbolISOamsaleft right double arrow
    ("forall" . "\u2200") ; (8704)HTML 4.0HTMLsymbolISOtechfor all
    ("part" . "\u2202") ; (8706)HTML 4.0HTMLsymbolISOtechpartial differential
    ("exist" . "\u2203") ; (8707)HTML 4.0HTMLsymbolISOtechthere exists
    ("empty" . "\u2205") ; (8709)HTML 4.0HTMLsymbolISOamsoempty set (= null set = diameter)
    ("nabla" . "\u2207") ; (8711)HTML 4.0HTMLsymbolISOtechnabla (= backward difference)
    ("isin" . "\u2208") ; (8712)HTML 4.0HTMLsymbolISOtechelement of
    ("notin" . "\u2209") ; (8713)HTML 4.0HTMLsymbolISOtechnot an element of
    ("ni" . "\u220B") ; (8715)HTML 4.0HTMLsymbolISOtechcontains as member
    ("prod" . "\u220F") ; (8719)HTML 4.0HTMLsymbolISOamsbn-ary product (= product sign)[k]
    ("sum" . "\u2211") ; (8721)HTML 4.0HTMLsymbolISOamsbn-ary summation[l]
    ("minus" . "\u2212") ; (8722)HTML 4.0HTMLsymbolISOtechminus sign
    ("lowast" . "\u2217") ; (8727)HTML 4.0HTMLsymbolISOtechasterisk operator
    ("radic" . "\u221A") ; (8730)HTML 4.0HTMLsymbolISOtechsquare root (= radical sign)
    ("prop" . "\u221D") ; (8733)HTML 4.0HTMLsymbolISOtechproportional to
    ("infin" . "\u221E") ; (8734)HTML 4.0HTMLsymbolISOtechinfinity
    ("ang" . "\u2220") ; (8736)HTML 4.0HTMLsymbolISOamsoangle
    ("and" . "\u2227") ; (8743)HTML 4.0HTMLsymbolISOtechlogical and (= wedge)
    ("or" . "\u2228") ; (8744)HTML 4.0HTMLsymbolISOtechlogical or (= vee)
    ("cap" . "\u2229") ; (8745)HTML 4.0HTMLsymbolISOtechintersection (= cap)
    ("cup" . "\u222A") ; (8746)HTML 4.0HTMLsymbolISOtechunion (= cup)
    ("int" . "\u222B") ; (8747)HTML 4.0HTMLsymbolISOtechintegral
    ("there4" . "\u2234") ; (8756)HTML 4.0HTMLsymbolISOtechtherefore sign
    ("sim" . "\u223C") ; (8764)HTML 4.0HTMLsymbolISOtechtilde operator (= varies with = similar to)[m]
    ("cong" . "\u2245") ; (8773)HTML 4.0HTMLsymbolISOtechcongruent to
    ("asymp" . "\u2248") ; (8776)HTML 4.0HTMLsymbolISOamsralmost equal to (= asymptotic to)
    ("ne" . "\u2260") ; (8800)HTML 4.0HTMLsymbolISOtechnot equal to
    ("equiv" . "\u2261") ; (8801)HTML 4.0HTMLsymbolISOtechidentical to; sometimes used for 'equivalent to'
    ("le" . "\u2264") ; (8804)HTML 4.0HTMLsymbolISOtechless-than or equal to
    ("ge" . "\u2265") ; (8805)HTML 4.0HTMLsymbolISOtechgreater-than or equal to
    ("sub" . "\u2282") ; (8834)HTML 4.0HTMLsymbolISOtechsubset of
    ("sup" . "\u2283") ; (8835)HTML 4.0HTMLsymbolISOtechsuperset of[n]
    ("nsub" . "\u2284") ; (8836)HTML 4.0HTMLsymbolISOamsnnot a subset of
    ("sube" . "\u2286") ; (8838)HTML 4.0HTMLsymbolISOtechsubset of or equal to
    ("supe" . "\u2287") ; (8839)HTML 4.0HTMLsymbolISOtechsuperset of or equal to
    ("oplus" . "\u2295") ; (8853)HTML 4.0HTMLsymbolISOamsbcircled plus (= direct sum)
    ("otimes" . "\u2297") ; (8855)HTML 4.0HTMLsymbolISOamsbcircled times (= vector product)
    ("perp" . "\u22A5") ; (8869)HTML 4.0HTMLsymbolISOtechup tack (= orthogonal to = perpendicular)[o]
    ("sdot" . "\u22C5") ; (8901)HTML 4.0HTMLsymbolISOamsbdot operator[p]
    ("lceil" . "\u2308") ; (8968)HTML 4.0HTMLsymbolISOamscleft ceiling (= APL upstile)
    ("rceil" . "\u2309") ; (8969)HTML 4.0HTMLsymbolISOamscright ceiling
    ("lfloor" . "\u230A") ; (8970)HTML 4.0HTMLsymbolISOamscleft floor (= APL downstile)
    ("rfloor" . "\u230B") ; (8971)HTML 4.0HTMLsymbolISOamscright floor
    ("lang" . "\u2329") ; (9001)HTML 4.0HTMLsymbolISOtechleft-pointing angle bracket (= bra)[q]
    ("rang" . "\u232A") ; (9002)HTML 4.0HTMLsymbolISOtechright-pointing angle bracket (= ket)[r]
    ("loz" . "\u25CA") ; (9674)HTML 4.0HTMLsymbolISOpublozenge
    ("spades" . "\u2660") ; (9824)HTML 4.0HTMLsymbolISOpubblack spade suit[f]
    ("clubs" . "\u2663") ; (9827)HTML 4.0HTMLsymbolISOpubblack club suit (= shamrock)[f]
    ("hearts" . "\u2665") ; (9829)HTML 4.0HTMLsymbolISOpubblack heart suit (= valentine)[f]
    ("diams" . "\u2666"))) ; (9830)HTML 4.0HTMLsymbolISOpubblack diamond suit[f]

(define (is-html-entity-name s)
  (empty?
   (filter 
    not  
    (map (lambda (x) (char-set-contains? char-set:letter x)) 
	 (string->list s)))))

(define (html-entities-decode string #!optional (start 0))
  (let ((pos (string-index string #\& start)))
    (if (not pos)
	string
	(let ((boundary (string-index string #\; pos)))
	  (if (not boundary)
	      string
	      (let ((entity (substring/shared string (+ pos 1) boundary)))
		(if (not (is-html-entity-name entity))
		    (html-entities-decode string (+ pos 1))
		    (let ((rep (or 
				(cdr (assoc (string-downcase entity) *html-entities*))
				entity)))
		      (html-entities-decode (string-replace string rep pos (+ boundary 1))
					    (+ pos 1))))))))))


	      