package com.github.morotsman.lote.internal.util

object Symbols {

  // === Box Drawing (Thin) ===
  val boxThin: Map[String, Char] = Map(
    "horizontal" -> '─',
    "vertical" -> '│',
    "topLeft" -> '┌',
    "topRight" -> '┐',
    "bottomLeft" -> '└',
    "bottomRight" -> '┘',
    "tRight" -> '├',
    "tLeft" -> '┤',
    "tDown" -> '┬',
    "tUp" -> '┴',
    "cross" -> '┼'
  )

  // === Box Drawing (Heavy) ===
  val boxHeavy: Map[String, Char] = Map(
    "horizontal" -> '━',
    "vertical" -> '┃',
    "topLeft" -> '┏',
    "topRight" -> '┓',
    "bottomLeft" -> '┗',
    "bottomRight" -> '┛',
    "tRight" -> '┣',
    "tLeft" -> '┫',
    "tDown" -> '┳',
    "tUp" -> '┻',
    "cross" -> '╋'
  )

  // === Box Drawing (Double) ===
  val boxDouble: Map[String, Char] = Map(
    "horizontal" -> '═',
    "vertical" -> '║',
    "topLeft" -> '╔',
    "topRight" -> '╗',
    "bottomLeft" -> '╚',
    "bottomRight" -> '╝',
    "tRight" -> '╠',
    "tLeft" -> '╣',
    "tDown" -> '╦',
    "tUp" -> '╩',
    "cross" -> '╬'
  )

  // === Box Drawing (Rounded) ===
  val boxRounded: Map[String, Char] = Map(
    "topLeft" -> '╭',
    "topRight" -> '╮',
    "bottomLeft" -> '╰',
    "bottomRight" -> '╯'
  )

  // === Box Drawing (Dashed) ===
  val boxDashed: Map[String, Char] = Map(
    "lightDashH" -> '┄',
    "heavyDashH" -> '┅',
    "lightDashV" -> '┆',
    "heavyDashV" -> '┇',
    "lightDashH4" -> '┈',
    "heavyDashH4" -> '┉',
    "lightDashV4" -> '┊',
    "heavyDashV4" -> '┋'
  )

  // === Block Elements ===
  val blocks: Map[String, Char] = Map(
    "lightShade" -> '░',
    "mediumShade" -> '▒',
    "darkShade" -> '▓',
    "fullBlock" -> '█',
    "upperHalf" -> '▀',
    "lowerHalf" -> '▄',
    "leftHalf" -> '▌',
    "rightHalf" -> '▐',
    "leftEighth" -> '▏',
    "leftQuarter" -> '▎',
    "left3Eighths" -> '▍',
    "left5Eighths" -> '▋',
    "left3Quarter" -> '▊',
    "left7Eighths" -> '▉'
  )

  // === Arrows ===
  val arrows: Map[String, Char] = Map(
    "left" -> '←',
    "right" -> '→',
    "up" -> '↑',
    "down" -> '↓',
    "leftRight" -> '↔',
    "upDown" -> '↕',
    "upLeft" -> '↖',
    "upRight" -> '↗',
    "downRight" -> '↘',
    "downLeft" -> '↙',
    "doubleLeft" -> '⇐',
    "doubleRight" -> '⇒',
    "doubleUp" -> '⇑',
    "doubleDown" -> '⇓',
    "doubleLeftRight" -> '⇔',
    "doubleUpDown" -> '⇕',
    "triangleUp" -> '▲',
    "triangleDown" -> '▼',
    "triangleLeft" -> '◀',
    "triangleRight" -> '▶',
    "triangleUpEmpty" -> '△',
    "triangleDownEmpty" -> '▽',
    "triangleLeftEmpty" -> '◁',
    "triangleRightEmpty" -> '▷'
  )

  // === Geometric Shapes ===
  val geometric: Map[String, Char] = Map(
    "squareFilled" -> '■',
    "squareEmpty" -> '□',
    "squareSmFilled" -> '▪',
    "squareSmEmpty" -> '▫',
    "circleFilled" -> '●',
    "circleEmpty" -> '○',
    "diamondFilled" -> '◆',
    "diamondEmpty" -> '◇',
    "circleDot" -> '◉',
    "circleDouble" -> '◎',
    "starFilled" -> '★',
    "starEmpty" -> '☆'
  )

  // === Checkmarks & Crosses ===
  val checks: Map[String, Char] = Map(
    "checkLight" -> '✓',
    "crossLight" -> '✗',
    "checkHeavy" -> '✔',
    "crossHeavy" -> '✘',
    "crossMultiply" -> '✕',
    "crossBold" -> '✖',
    "plusHeavy" -> '✚',
    "plusCross" -> '✜'
  )

  // === Braille (spinner frames) ===
  val braille: Map[String, Char] = Map(
    "dot1" -> '⠋',
    "dot2" -> '⠙',
    "dot3" -> '⠹',
    "dot4" -> '⠸',
    "dot5" -> '⠼',
    "dot6" -> '⠴',
    "dot7" -> '⠦',
    "dot8" -> '⠧',
    "dot9" -> '⠇',
    "dot10" -> '⠏',
    "full1" -> '⣾',
    "full2" -> '⣽',
    "full3" -> '⣻',
    "full4" -> '⢿',
    "full5" -> '⡿',
    "full6" -> '⣟',
    "full7" -> '⣯',
    "full8" -> '⣷',
    "fullBlock" -> '⣿',
    "emptyBlock" -> '⣀'
  )

  // === Mac/Keyboard Symbols ===
  val keyboard: Map[String, Char] = Map(
    "returnKey" -> '⏎',
    "command" -> '⌘',
    "option" -> '⌥',
    "shift" -> '⇧',
    "control" -> '⌃',
    "escape" -> '⎋',
    "backspace" -> '⌫',
    "delete" -> '⌦',
    "tab" -> '⇥',
    "backTab" -> '⇤'
  )

  // === Card Suits ===
  val cards: Map[String, Char] = Map(
    "spadeFilled" -> '♠',
    "clubFilled" -> '♣',
    "heartFilled" -> '♥',
    "diamondFilled" -> '♦',
    "spadeEmpty" -> '♤',
    "clubEmpty" -> '♧',
    "heartEmpty" -> '♡',
    "diamondEmpty" -> '♢'
  )

  // === Checkboxes ===
  val checkboxes: Map[String, Char] = Map(
    "unchecked" -> '☐',
    "checked" -> '☑',
    "crossed" -> '☒'
  )

  // === Weather / Miscellaneous ===
  val misc: Map[String, Char] = Map(
    "lightning" -> '⚡',
    "warning" -> '⚠',
    "gear" -> '⚙',
    "flagFilled" -> '⚑',
    "flagEmpty" -> '⚐',
    "sun" -> '☀',
    "cloud" -> '☁',
    "umbrella" -> '☂',
    "snowman" -> '☃',
    "infinity" -> '∞',
    "ellipsis" -> '…',
    "bullet" -> '•',
    "degree" -> '°',
    "section" -> '§',
    "paragraph" -> '¶',
    "copyright" -> '©',
    "registered" -> '®',
    "trademark" -> '™'
  )

  // === Math / Logic ===
  val math: Map[String, Char] = Map(
    "plusMinus" -> '±',
    "multiply" -> '×',
    "divide" -> '÷',
    "notEqual" -> '≠',
    "lessOrEqual" -> '≤',
    "greaterOrEqual" -> '≥',
    "approx" -> '≈',
    "infinity" -> '∞',
    "squareRoot" -> '√',
    "sum" -> '∑',
    "product" -> '∏',
    "integral" -> '∫',
    "partial" -> '∂',
    "delta" -> '∆',
    "nabla" -> '∇',
    "elementOf" -> '∈',
    "notElementOf" -> '∉',
    "emptySet" -> '∅',
    "logicalAnd" -> '∧',
    "logicalOr" -> '∨',
    "logicalNot" -> '¬'
  )

  // === Musical Notes ===
  val music: Map[String, Char] = Map(
    "quarterNote" -> '♩',
    "eighthNote" -> '♪',
    "beamedNotes" -> '♫',
    "beamedSixteenth" -> '♬'
  )

  // === Zodiac ===
  val zodiac: Map[String, Char] = Map(
    "aries" -> '♈',
    "taurus" -> '♉',
    "gemini" -> '♊',
    "cancer" -> '♋',
    "leo" -> '♌',
    "virgo" -> '♍',
    "libra" -> '♎',
    "scorpio" -> '♏',
    "sagittarius" -> '♐',
    "capricorn" -> '♑',
    "aquarius" -> '♒',
    "pisces" -> '♓'
  )

  // === Currency ===
  val currency: Map[String, Char] = Map(
    "dollar" -> '$',
    "euro" -> '€',
    "pound" -> '£',
    "yen" -> '¥',
    "rupee" -> '₹',
    "ruble" -> '₽',
    "bitcoin" -> '₿',
    "cent" -> '¢',
    "won" -> '₩',
    "dong" -> '₫',
    "hryvnia" -> '₴',
    "lira" -> '₺',
    "peso" -> '₱',
    "tenge" -> '₸',
    "colon" -> '₡',
    "naira" -> '₦'
  )

  // === Superscript ===
  val superscript: Map[String, Char] = Map(
    "0" -> '⁰',
    "1" -> '¹',
    "2" -> '²',
    "3" -> '³',
    "4" -> '⁴',
    "5" -> '⁵',
    "6" -> '⁶',
    "7" -> '⁷',
    "8" -> '⁸',
    "9" -> '⁹',
    "plus" -> '⁺',
    "minus" -> '⁻',
    "equal" -> '⁼',
    "leftParen" -> '⁽',
    "rightParen" -> '⁾',
    "n" -> 'ⁿ'
  )

  // === Subscript ===
  val subscript: Map[String, Char] = Map(
    "0" -> '₀',
    "1" -> '₁',
    "2" -> '₂',
    "3" -> '₃',
    "4" -> '₄',
    "5" -> '₅',
    "6" -> '₆',
    "7" -> '₇',
    "8" -> '₈',
    "9" -> '₉',
    "plus" -> '₊',
    "minus" -> '₋',
    "equal" -> '₌',
    "leftParen" -> '₍',
    "rightParen" -> '₎'
  )

  // === Fractions ===
  val fractions: Map[String, Char] = Map(
    "oneHalf" -> '½',
    "oneThird" -> '⅓',
    "twoThirds" -> '⅔',
    "oneQuarter" -> '¼',
    "threeQuarters" -> '¾',
    "oneFifth" -> '⅕',
    "twoFifths" -> '⅖',
    "threeFifths" -> '⅗',
    "fourFifths" -> '⅘',
    "oneSixth" -> '⅙',
    "fiveSixths" -> '⅚',
    "oneEighth" -> '⅛',
    "threeEighths" -> '⅜',
    "fiveEighths" -> '⅝',
    "sevenEighths" -> '⅞'
  )

  // === Roman Numerals ===
  val roman: Map[String, Char] = Map(
    "I" -> 'Ⅰ',
    "II" -> 'Ⅱ',
    "III" -> 'Ⅲ',
    "IV" -> 'Ⅳ',
    "V" -> 'Ⅴ',
    "VI" -> 'Ⅵ',
    "VII" -> 'Ⅶ',
    "VIII" -> 'Ⅷ',
    "IX" -> 'Ⅸ',
    "X" -> 'Ⅹ',
    "XI" -> 'Ⅺ',
    "XII" -> 'Ⅻ'
  )

  // === Circled Numbers ===
  val circledNumbers: Map[String, Char] = Map(
    "1" -> '①',
    "2" -> '②',
    "3" -> '③',
    "4" -> '④',
    "5" -> '⑤',
    "6" -> '⑥',
    "7" -> '⑦',
    "8" -> '⑧',
    "9" -> '⑨',
    "10" -> '⑩',
    "11" -> '⑪',
    "12" -> '⑫',
    "13" -> '⑬',
    "14" -> '⑭',
    "15" -> '⑮',
    "16" -> '⑯',
    "17" -> '⑰',
    "18" -> '⑱',
    "19" -> '⑲',
    "20" -> '⑳'
  )

  // === Filled Circled Numbers ===
  val circledNumbersFilled: Map[String, Char] = Map(
    "1" -> '❶',
    "2" -> '❷',
    "3" -> '❸',
    "4" -> '❹',
    "5" -> '❺',
    "6" -> '❻',
    "7" -> '❼',
    "8" -> '❽',
    "9" -> '❾',
    "10" -> '❿'
  )

  // === Circled Letters ===
  val circledLetters: Map[String, Char] = Map(
    "A" -> 'Ⓐ',
    "B" -> 'Ⓑ',
    "C" -> 'Ⓒ',
    "D" -> 'Ⓓ',
    "E" -> 'Ⓔ',
    "F" -> 'Ⓕ',
    "G" -> 'Ⓖ',
    "H" -> 'Ⓗ',
    "I" -> 'Ⓘ',
    "J" -> 'Ⓙ',
    "K" -> 'Ⓚ',
    "L" -> 'Ⓛ',
    "M" -> 'Ⓜ',
    "N" -> 'Ⓝ',
    "O" -> 'Ⓞ',
    "P" -> 'Ⓟ',
    "Q" -> 'Ⓠ',
    "R" -> 'Ⓡ',
    "S" -> 'Ⓢ',
    "T" -> 'Ⓣ',
    "U" -> 'Ⓤ',
    "V" -> 'Ⓥ',
    "W" -> 'Ⓦ',
    "X" -> 'Ⓧ',
    "Y" -> 'Ⓨ',
    "Z" -> 'Ⓩ'
  )

  // === Dingbats / Ornaments ===
  val dingbats: Map[String, Char] = Map(
    "diamond4" -> '❖',
    "flower1" -> '❀',
    "flower2" -> '❁',
    "flower3" -> '❂',
    "flower4" -> '❃',
    "florette1" -> '❊',
    "florette2" -> '❋',
    "flower5" -> '✾',
    "flower6" -> '✿',
    "snowflake1" -> '❆',
    "snowflake2" -> '❅',
    "snowflake3" -> '❄',
    "star4pointed" -> '✦',
    "star4empty" -> '✧',
    "star5stress" -> '✩',
    "star6white" -> '✪',
    "star5open" -> '✫',
    "star6open" -> '✬',
    "star5filled" -> '✭',
    "star6pointed" -> '✮',
    "star8" -> '✯',
    "starShadow" -> '✰'
  )

  // === Dingbat Arrows ===
  val dingbatArrows: Map[String, Char] = Map(
    "arrowRight1" -> '➔',
    "arrowRight2" -> '➜',
    "arrowRight3" -> '➝',
    "arrowRight4" -> '➞',
    "arrowRight5" -> '➟',
    "arrowRight6" -> '➠',
    "arrowRight7" -> '➡',
    "arrowRight8" -> '➢',
    "arrowRight9" -> '➣',
    "arrowRight10" -> '➤',
    "arrowRight11" -> '➥',
    "arrowRight12" -> '➦',
    "arrowRight13" -> '➧',
    "arrowRight14" -> '➨',
    "arrowRight15" -> '➩',
    "arrowRight16" -> '➪',
    "arrowRight17" -> '➫',
    "arrowRight18" -> '➬',
    "arrowRight19" -> '➭',
    "arrowRight20" -> '➮',
    "arrowRight21" -> '➯',
    "arrowRight22" -> '➱'
  )

  // === Punctuation / Typography ===
  val typography: Map[String, Char] = Map(
    "enDash" -> '–',
    "emDash" -> '—',
    "leftSingleQ" -> '\u2018',
    "rightSingleQ" -> '\u2019',
    "leftDoubleQ" -> '\u201C',
    "rightDoubleQ" -> '\u201D',
    "singleLeftAngle" -> '‹',
    "singleRightAngle" -> '›',
    "doubleLeftAngle" -> '«',
    "doubleRightAngle" -> '»',
    "hyphen" -> '‐',
    "nonBreakHyphen" -> '‑',
    "figureDash" -> '‒',
    "dagger" -> '†',
    "doubleDagger" -> '‡',
    "perMille" -> '‰',
    "prime" -> '′',
    "doublePrime" -> '″',
    "triplePrime" -> '‴'
  )

  // === Greek Alphabet (uppercase) ===
  val greekUpper: Map[String, Char] = Map(
    "Alpha" -> 'Α',
    "Beta" -> 'Β',
    "Gamma" -> 'Γ',
    "Delta" -> 'Δ',
    "Epsilon" -> 'Ε',
    "Zeta" -> 'Ζ',
    "Eta" -> 'Η',
    "Theta" -> 'Θ',
    "Iota" -> 'Ι',
    "Kappa" -> 'Κ',
    "Lambda" -> 'Λ',
    "Mu" -> 'Μ',
    "Nu" -> 'Ν',
    "Xi" -> 'Ξ',
    "Omicron" -> 'Ο',
    "Pi" -> 'Π',
    "Rho" -> 'Ρ',
    "Sigma" -> 'Σ',
    "Tau" -> 'Τ',
    "Upsilon" -> 'Υ',
    "Phi" -> 'Φ',
    "Chi" -> 'Χ',
    "Psi" -> 'Ψ',
    "Omega" -> 'Ω'
  )

  // === Greek Alphabet (lowercase) ===
  val greekLower: Map[String, Char] = Map(
    "alpha" -> 'α',
    "beta" -> 'β',
    "gamma" -> 'γ',
    "delta" -> 'δ',
    "epsilon" -> 'ε',
    "zeta" -> 'ζ',
    "eta" -> 'η',
    "theta" -> 'θ',
    "iota" -> 'ι',
    "kappa" -> 'κ',
    "lambda" -> 'λ',
    "mu" -> 'μ',
    "nu" -> 'ν',
    "xi" -> 'ξ',
    "omicron" -> 'ο',
    "pi" -> 'π',
    "rho" -> 'ρ',
    "sigma" -> 'σ',
    "tau" -> 'τ',
    "upsilon" -> 'υ',
    "phi" -> 'φ',
    "chi" -> 'χ',
    "psi" -> 'ψ',
    "omega" -> 'ω'
  )

  // === Powerline Symbols (requires Nerd Font / Powerline font) ===
  val powerline: Map[String, Char] = Map(
    "rightTriangle" -> '\uE0B0',
    "rightTriangleThin" -> '\uE0B1',
    "leftTriangle" -> '\uE0B2',
    "leftTriangleThin" -> '\uE0B3'
  )

  // === Miscellaneous Symbols ===
  val miscExtra: Map[String, Char] = Map(
    "hourglass" -> '⌛',
    "watch" -> '⌚',
    "phone" -> '☎',
    "envelope" -> '✉',
    "scissors" -> '✂',
    "airplane" -> '✈',
    "peace" -> '☮',
    "yinYang" -> '☯',
    "starAndCrescent" -> '☪',
    "starOfDavid" -> '✡',
    "dharmaWheel" -> '☸',
    "recycle" -> '♻',
    "atom" -> '⚛',
    "caduceus" -> '⚕',
    "skullBones" -> '☠',
    "swords" -> '⚔',
    "scales" -> '⚖',
    "alembic" -> '⚗',
    "flowerOf" -> '⚘'
  )

  // === Circle Variants ===
  val circles: Map[String, Char] = Map(
    "dottedCircle" -> '◌',
    "circleMedium" -> '◍',
    "circleLeftHalf" -> '◐',
    "circleRightHalf" -> '◑',
    "circleLowerHalf" -> '◒',
    "circleUpperHalf" -> '◓',
    "circleUpperRight" -> '◔',
    "circle3Quarter" -> '◕',
    "circleLeftBlack" -> '◖',
    "circleRightBlack" -> '◗'
  )

  // === Technical / Media ===
  val technical: Map[String, Char] = Map(
    "eject" -> '⏏',
    "verticalLine" -> '⏐',
    "metrical1" -> '⏑',
    "metrical2" -> '⏒',
    "metrical3" -> '⏓',
    "metrical4" -> '⏔',
    "metrical5" -> '⏕',
    "ground" -> '⏚',
    "fuse" -> '⏛',
    "topArc" -> '⏜',
    "bottomArc" -> '⏝',
    "topBrace" -> '⏞',
    "bottomBrace" -> '⏟',
    "topTortoise" -> '⏠',
    "bottomTortoise" -> '⏡'
  )

  // === CJK Brackets ===
  val cjkBrackets: Map[String, Char] = Map(
    "blackLentLeft" -> '【',
    "blackLentRight" -> '】',
    "whiteLentLeft" -> '〖',
    "whiteLentRight" -> '〗',
    "whiteTortoiseLeft" -> '〘',
    "whiteTortoiseRight" -> '〙',
    "whiteSquareLeft" -> '〚',
    "whiteSquareRight" -> '〛',
    "cornerLeft" -> '「',
    "cornerRight" -> '」',
    "doubleCornerLeft" -> '『',
    "doubleCornerRight" -> '』'
  )

  // === Pixel Simulation: Quadrant Blocks (2x2 per cell) ===
  // Each cell is divided into 4 quadrants: upper-left, upper-right, lower-left, lower-right
  val quadrants: Map[String, Char] = Map(
    "upperLeft" -> '▘',
    "upperRight" -> '▝',
    "lowerLeft" -> '▖',
    "lowerRight" -> '▗',
    "upperLeftAndRight" -> '▀', // top half
    "lowerLeftAndRight" -> '▄', // bottom half
    "upperAndLowerLeft" -> '▌', // left half
    "upperAndLowerRight" -> '▐', // right half
    "diagonalDown" -> '▚', // upper-left + lower-right
    "diagonalUp" -> '▞', // upper-right + lower-left
    "noUpperRight" -> '▙', // all except upper-right
    "noLowerRight" -> '▛', // all except lower-right
    "noLowerLeft" -> '▜', // all except lower-left
    "noUpperLeft" -> '▟', // all except upper-left
    "full" -> '█' // all quadrants filled
  )

  // === Pixel Simulation: Braille (2x4 per cell = 8 sub-pixels) ===
  // The BEST option for high-resolution terminal graphics.
  // Base char is U+2800 (⠀ = blank). Add dot values to enable pixels:
  //   Dot layout:    Values:
  //     ①④            0x01  0x08
  //     ②⑤            0x02  0x10
  //     ③⑥            0x04  0x20
  //     ⑦⑧            0x40  0x80
  //
  // To compose: char = (0x2800 + sum_of_dot_values).toChar
  // Example: top-left dot = 0x2800 + 0x01 = '⠁'
  //          all dots     = 0x2800 + 0xFF = '⣿'
  val braillePixels: Map[String, Char] = Map(
    "blank" -> '⠀', // no dots (U+2800)
    // Individual dots
    "dot1_topLeft" -> '⠁', // 0x01
    "dot2_midLeft" -> '⠂', // 0x02
    "dot3_lowLeft" -> '⠄', // 0x04
    "dot4_topRight" -> '⠈', // 0x08
    "dot5_midRight" -> '⠐', // 0x10
    "dot6_lowRight" -> '⠠', // 0x20
    "dot7_bottomLeft" -> '⡀', // 0x40
    "dot8_bottomRight" -> '⢀', // 0x80
    // Common combinations
    "topRow" -> '⠉', // dots 1+4
    "midRow" -> '⠒', // dots 2+5
    "lowRow" -> '⠤', // dots 3+6
    "bottomRow" -> '⣀', // dots 7+8
    "leftCol" -> '⡇', // dots 1+2+3+7
    "rightCol" -> '⢸', // dots 4+5+6+8
    "topHalf" -> '⠛', // dots 1+2+4+5
    "bottomHalf" -> '⣤', // dots 3+6+7+8  (actually ⣠+more)
    "full" -> '⣿', // all 8 dots (0xFF)
    "leftHalf" -> '⡇', // left column all
    "rightHalf" -> '⢸', // right column all
    "outline" -> '⣏', // border-like
    "cross" -> '⠿', // dots 1-6
    "topLeft2x2" -> '⠋', // dots 1+2+4
    "border" -> '⣹' // partial border
  )

  /** Helper: compose a braille character from individual pixel flags */
  object BraillePixel {
    // Dot bit values for each position in a 2x4 grid
    // Position (col, row) where col=0,1 and row=0,1,2,3
    val dotBits: Map[(Int, Int), Int] = Map(
      (0, 0) -> 0x01, // top-left
      (0, 1) -> 0x02, // mid-left
      (0, 2) -> 0x04, // lower-left
      (1, 0) -> 0x08, // top-right
      (1, 1) -> 0x10, // mid-right
      (1, 2) -> 0x20, // lower-right
      (0, 3) -> 0x40, // bottom-left
      (1, 3) -> 0x80 // bottom-right
    )

    /** Create a braille char from a set of (col, row) pixel positions */
    def fromPixels(pixels: Set[(Int, Int)]): Char =
      (0x2800 + pixels.flatMap(dotBits.get).sum).toChar

    /** Create a braille char from a bitmask (0x00 to 0xFF) */
    def fromBits(bits: Int): Char = (0x2800 + (bits & 0xff)).toChar

    /** All 256 braille characters */
    val allChars: IndexedSeq[Char] = (0 to 255).map(i => (0x2800 + i).toChar)
  }

  // === Pixel Simulation: Sextant Characters (2x3 per cell = 6 sub-pixels) ===
  // Unicode 13.0+ "Symbols for Legacy Computing" U+1FB00–U+1FB3B
  // These may not render in all terminals/fonts
  val sextants: Map[String, String] = Map(
    "topLeft" -> "🬀",
    "topRight" -> "🬁",
    "topBoth" -> "🬂",
    "midLeft" -> "🬃",
    "midRight" -> "🬄",
    "topLeftMidLeft" -> "🬅",
    "topRightMidLeft" -> "🬆",
    "topBothMidLeft" -> "🬇",
    "topLeftMidRight" -> "🬈",
    "topRightMidRight" -> "🬉",
    "topBothMidRight" -> "🬊",
    "midBoth" -> "🬋",
    "topLeftMidBoth" -> "🬌",
    "topRightMidBoth" -> "🬍",
    "topBothMidBoth" -> "🬎",
    "botLeft" -> "🬏",
    "botRight" -> "🬐"
    // ... continues through U+1FB3B for all 2x3 combinations
  )

  // === Additional Box Drawing ===
  val boxExtra: Map[String, Char] = Map(
    "lightDash2H" -> '╌',
    "heavyDash2H" -> '╍',
    "lightDash2V" -> '╎',
    "heavyDash2V" -> '╏',
    "lightLeftStub" -> '╴',
    "lightUpStub" -> '╵',
    "lightRightStub" -> '╶',
    "lightDownStub" -> '╷',
    "heavyLeftStub" -> '╸',
    "heavyUpStub" -> '╹',
    "heavyRightStub" -> '╺',
    "heavyDownStub" -> '╻',
    "lightLeftHeavyRight" -> '╼',
    "lightUpHeavyDown" -> '╽',
    "heavyLeftLightRight" -> '╾',
    "heavyUpLightDown" -> '╿'
  )

  // === Emoji (multi-byte, stored as String) ===

  // --- Faces & Emotions ---
  val emojiFaces: Map[String, String] = Map(
    "grinning" -> "😀",
    "grinningEyes" -> "😁",
    "joy" -> "😂",
    "rofl" -> "🤣",
    "smilingEyes" -> "😊",
    "halo" -> "😇",
    "wink" -> "😉",
    "blush" -> "😊",
    "savoring" -> "😋",
    "sunglasses" -> "😎",
    "heartEyes" -> "😍",
    "kissing" -> "😘",
    "thinking" -> "🤔",
    "zipperMouth" -> "🤐",
    "raisedEyebrow" -> "🤨",
    "neutral" -> "😐",
    "expressionless" -> "😑",
    "unamused" -> "😒",
    "rollingEyes" -> "🙄",
    "grimacing" -> "😬",
    "lying" -> "🤥",
    "relieved" -> "😌",
    "sleepy" -> "😪",
    "drooling" -> "🤤",
    "sleeping" -> "😴",
    "mask" -> "😷",
    "thermometer" -> "🤒",
    "bandage" -> "🤕",
    "nauseated" -> "🤢",
    "vomiting" -> "🤮",
    "sneezing" -> "🤧",
    "hot" -> "🥵",
    "cold" -> "🥶",
    "woozy" -> "🥴",
    "dizzy" -> "😵",
    "explodingHead" -> "🤯",
    "cowboy" -> "🤠",
    "partying" -> "🥳",
    "nerd" -> "🤓",
    "monocle" -> "🧐",
    "confused" -> "😕",
    "worried" -> "😟",
    "slightFrown" -> "🙁",
    "frown" -> "☹️",
    "openMouth" -> "😮",
    "hushed" -> "😯",
    "astonished" -> "😲",
    "flushed" -> "😳",
    "pleading" -> "🥺",
    "crying" -> "😢",
    "sobbing" -> "😭",
    "scream" -> "😱",
    "confounded" -> "😖",
    "persevering" -> "😣",
    "disappointed" -> "😞",
    "sweat" -> "😓",
    "weary" -> "😩",
    "tired" -> "😫",
    "yawning" -> "🥱",
    "angry" -> "😠",
    "rage" -> "😡",
    "cursing" -> "🤬",
    "smiling_devil" -> "😈",
    "devil" -> "👿",
    "skull" -> "💀",
    "skullCrossbones" -> "☠️",
    "poop" -> "💩",
    "clown" -> "🤡",
    "ogre" -> "👹",
    "goblin" -> "👺",
    "ghost" -> "👻",
    "alien" -> "👽",
    "alienMonster" -> "👾",
    "robot" -> "🤖"
  )

  // --- Hands & Gestures ---
  val emojiHands: Map[String, String] = Map(
    "wave" -> "👋",
    "raisedBack" -> "🤚",
    "raisedHand" -> "✋",
    "vulcan" -> "🖖",
    "ok" -> "👌",
    "pinching" -> "🤏",
    "victory" -> "✌️",
    "crossedFingers" -> "🤞",
    "loveyou" -> "🤟",
    "rockOn" -> "🤘",
    "callMe" -> "🤙",
    "pointLeft" -> "👈",
    "pointRight" -> "👉",
    "pointUp" -> "👆",
    "middleFinger" -> "🖕",
    "pointDown" -> "👇",
    "thumbsUp" -> "👍",
    "thumbsDown" -> "👎",
    "fist" -> "✊",
    "fistBump" -> "👊",
    "leftFist" -> "🤛",
    "rightFist" -> "🤜",
    "clap" -> "👏",
    "raisedHands" -> "🙌",
    "openHands" -> "👐",
    "palmsUp" -> "🤲",
    "handshake" -> "🤝",
    "pray" -> "🙏",
    "writingHand" -> "✍️",
    "nail_polish" -> "💅",
    "selfie" -> "🤳",
    "flexed" -> "💪"
  )

  // --- Hearts & Love ---
  val emojiHearts: Map[String, String] = Map(
    "redHeart" -> "❤️",
    "orangeHeart" -> "🧡",
    "yellowHeart" -> "💛",
    "greenHeart" -> "💚",
    "blueHeart" -> "💙",
    "purpleHeart" -> "💜",
    "blackHeart" -> "🖤",
    "whiteHeart" -> "🤍",
    "brownHeart" -> "🤎",
    "brokenHeart" -> "💔",
    "exclamationHeart" -> "❣️",
    "twoHearts" -> "💕",
    "revolvingHearts" -> "💞",
    "heartDecoration" -> "💟",
    "sparkling" -> "💖",
    "growing" -> "💗",
    "beating" -> "💓",
    "ribbon" -> "💝",
    "arrow" -> "💘",
    "kiss" -> "💋",
    "love_letter" -> "💌"
  )

  // --- Animals ---
  val emojiAnimals: Map[String, String] = Map(
    // Mammals
    "dog" -> "🐕",
    "dogFace" -> "🐶",
    "guideDog" -> "🦮",
    "serviceDog" -> "🐕‍🦺",
    "poodle" -> "🐩",
    "cat" -> "🐈",
    "catFace" -> "🐱",
    "blackCat" -> "🐈‍⬛",
    "mouse" -> "🐭",
    "mouseFull" -> "🐁",
    "rat" -> "🐀",
    "hamster" -> "🐹",
    "rabbit" -> "🐰",
    "rabbitFull" -> "🐇",
    "chipmunk" -> "🐿️",
    "beaver" -> "🦫",
    "hedgehog" -> "🦔",
    "fox" -> "🦊",
    "bear" -> "🐻",
    "polarBear" -> "🐻‍❄️",
    "panda" -> "🐼",
    "koala" -> "🐨",
    "sloth" -> "🦥",
    "otter" -> "🦦",
    "skunk" -> "🦨",
    "kangaroo" -> "🦘",
    "badger" -> "🦡",
    "tiger" -> "🐯",
    "tigerFull" -> "🐅",
    "lion" -> "🦁",
    "leopard" -> "🐆",
    "cow" -> "🐮",
    "cowFull" -> "🐄",
    "ox" -> "🐂",
    "waterBuffalo" -> "🐃",
    "bison" -> "🦬",
    "pig" -> "🐷",
    "pigFull" -> "🐖",
    "pigNose" -> "🐽",
    "boar" -> "🐗",
    "ram" -> "🐏",
    "sheep" -> "🐑",
    "goat" -> "🐐",
    "llama" -> "🦙",
    "camel" -> "🐫",
    "camelOne" -> "🐪",
    "hippopotamus" -> "🦛",
    "rhinoceros" -> "🦏",
    "elephant" -> "🐘",
    "mammoth" -> "🦣",
    "giraffe" -> "🦒",
    "zebra" -> "🦓",
    "deer" -> "🦌",
    "moose" -> "🫎",
    "donkey" -> "🫏",
    "horse" -> "🐴",
    "horseFull" -> "🐎",
    "unicorn" -> "🦄",
    "monkey" -> "🐵",
    "monkeyFull" -> "🐒",
    "gorilla" -> "🦍",
    "orangutan" -> "🦧",
    "wolf" -> "🐺",
    "dog2" -> "🐕",
    // Birds
    "chicken" -> "🐔",
    "rooster" -> "🐓",
    "hatchingChick" -> "🐣",
    "babyChick" -> "🐤",
    "frontChick" -> "🐥",
    "penguin" -> "🐧",
    "bird" -> "🐦",
    "eagle" -> "🦅",
    "duck" -> "🦆",
    "swan" -> "🦢",
    "owl" -> "🦉",
    "dodo" -> "🦤",
    "feather" -> "🪶",
    "flamingo" -> "🦩",
    "peacock" -> "🦚",
    "parrot" -> "🦜",
    "goose" -> "🪿",
    "bat" -> "🦇",
    "turkey" -> "🦃",
    "dove" -> "🕊️",
    // Reptiles & Amphibians
    "frog" -> "🐸",
    "crocodile" -> "🐊",
    "turtle" -> "🐢",
    "snake" -> "🐍",
    "lizard" -> "🦎",
    "dinosaur" -> "🦕",
    "trex" -> "🦖",
    "dragon" -> "🐉",
    "dragonFace" -> "🐲",
    // Marine
    "whale" -> "🐳",
    "whale2" -> "🐋",
    "dolphin" -> "🐬",
    "seal" -> "🦭",
    "fish" -> "🐟",
    "tropicalFish" -> "🐠",
    "blowfish" -> "🐡",
    "shark" -> "🦈",
    "octopus" -> "🐙",
    "squid" -> "🦑",
    "shell" -> "🐚",
    "coral" -> "🪸",
    "jellyfish" -> "🪼",
    // Crustaceans
    "shrimp" -> "🦐",
    "crab" -> "🦀",
    "lobster" -> "🦞",
    // Insects & Bugs
    "bee" -> "🐝",
    "bug" -> "🐛",
    "butterfly" -> "🦋",
    "snail" -> "🐌",
    "ladybug" -> "🐞",
    "ant" -> "🐜",
    "mosquito" -> "🦟",
    "fly" -> "🪰",
    "worm" -> "🪱",
    "cockroach" -> "🪳",
    "beetle" -> "🪲",
    "cricket" -> "🦗",
    "spider" -> "🕷️",
    "spiderWeb" -> "🕸️",
    "scorpion" -> "🦂",
    // Misc Animals
    "pawPrints" -> "🐾",
    "microbe" -> "🦠"
  )

  // --- Food & Drink ---
  val emojiFood: Map[String, String] = Map(
    "apple" -> "🍎",
    "greenApple" -> "🍏",
    "pear" -> "🍐",
    "orange" -> "🍊",
    "lemon" -> "🍋",
    "banana" -> "🍌",
    "watermelon" -> "🍉",
    "grapes" -> "🍇",
    "strawberry" -> "🍓",
    "cherry" -> "🍒",
    "peach" -> "🍑",
    "mango" -> "🥭",
    "pineapple" -> "🍍",
    "coconut" -> "🥥",
    "avocado" -> "🥑",
    "tomato" -> "🍅",
    "hotPepper" -> "🌶️",
    "corn" -> "🌽",
    "carrot" -> "🥕",
    "potato" -> "🥔",
    "bread" -> "🍞",
    "croissant" -> "🥐",
    "pretzel" -> "🥨",
    "cheese" -> "🧀",
    "egg" -> "🥚",
    "cooking" -> "🍳",
    "bacon" -> "🥓",
    "pancakes" -> "🥞",
    "hamburger" -> "🍔",
    "fries" -> "🍟",
    "pizza" -> "🍕",
    "hotdog" -> "🌭",
    "taco" -> "🌮",
    "burrito" -> "🌯",
    "sushi" -> "🍣",
    "ramen" -> "🍜",
    "spaghetti" -> "🍝",
    "iceCream" -> "🍦",
    "cake" -> "🎂",
    "pie" -> "🥧",
    "chocolate" -> "🍫",
    "candy" -> "🍬",
    "lollipop" -> "🍭",
    "donut" -> "🍩",
    "cookie" -> "🍪",
    "coffee" -> "☕",
    "tea" -> "🍵",
    "beer" -> "🍺",
    "beers" -> "🍻",
    "wine" -> "🍷",
    "cocktail" -> "🍸",
    "champagne" -> "🍾",
    "whiskey" -> "🥃",
    "juice" -> "🧃",
    "water" -> "💧"
  )

  // --- Nature & Weather ---
  val emojiNature: Map[String, String] = Map(
    "sun" -> "☀️",
    "sunFace" -> "🌞",
    "moon" -> "🌙",
    "fullMoon" -> "🌕",
    "newMoon" -> "🌑",
    "moonFace" -> "🌝",
    "star" -> "⭐",
    "glowingStar" -> "🌟",
    "shootingStar" -> "🌠",
    "cloud" -> "☁️",
    "sunBehindCloud" -> "⛅",
    "rainbow" -> "🌈",
    "umbrella" -> "☂️",
    "umbrellaRain" -> "🌂",
    "lightning" -> "⚡",
    "snowflake" -> "❄️",
    "fire" -> "🔥",
    "droplet" -> "💧",
    "ocean" -> "🌊",
    "tornado" -> "🌪️",
    "fog" -> "🌫️",
    "wind" -> "💨",
    "earth" -> "🌍",
    "earthAmericas" -> "🌎",
    "earthAsia" -> "🌏",
    "globe" -> "🌐",
    "volcano" -> "🌋",
    "mountain" -> "⛰️",
    "snowMountain" -> "🏔️",
    "tree" -> "🌳",
    "evergreenTree" -> "🌲",
    "palmTree" -> "🌴",
    "cactus" -> "🌵",
    "flower" -> "🌸",
    "rose" -> "🌹",
    "sunflower" -> "🌻",
    "herb" -> "🌿",
    "clover" -> "☘️",
    "fourLeafClover" -> "🍀",
    "mushroom" -> "🍄",
    "leaf" -> "🍃",
    "fallenLeaf" -> "🍂",
    "mapleLeaf" -> "🍁"
  )

  // --- Travel & Transport ---
  val emojiTravel: Map[String, String] = Map(
    "car" -> "🚗",
    "taxi" -> "🚕",
    "bus" -> "🚌",
    "trolleybus" -> "🚎",
    "ambulance" -> "🚑",
    "fireTruck" -> "🚒",
    "policeCar" -> "🚓",
    "truck" -> "🚚",
    "bicycle" -> "🚲",
    "motorcycle" -> "🏍️",
    "train" -> "🚆",
    "metro" -> "🚇",
    "tram" -> "🚊",
    "airplane" -> "✈️",
    "helicopter" -> "🚁",
    "rocket" -> "🚀",
    "satellite" -> "🛰️",
    "ship" -> "🚢",
    "sailboat" -> "⛵",
    "speedboat" -> "🚤",
    "anchor" -> "⚓",
    "fuel" -> "⛽",
    "construction" -> "🚧",
    "traffic_light" -> "🚦",
    "stopSign" -> "🛑",
    "compass" -> "🧭",
    "house" -> "🏠",
    "office" -> "🏢",
    "hospital" -> "🏥",
    "school" -> "🏫",
    "church" -> "⛪",
    "castle" -> "🏰",
    "tent" -> "⛺",
    "stadium" -> "🏟️",
    "ferrisWheel" -> "🎡",
    "rollerCoaster" -> "🎢"
  )

  // --- Objects & Tools ---
  val emojiObjects: Map[String, String] = Map(
    "watch" -> "⌚",
    "phone" -> "📱",
    "laptop" -> "💻",
    "desktop" -> "🖥️",
    "keyboard" -> "⌨️",
    "printer" -> "🖨️",
    "mouse" -> "🖱️",
    "disc" -> "💿",
    "floppy" -> "💾",
    "camera" -> "📷",
    "video" -> "📹",
    "television" -> "📺",
    "radio" -> "📻",
    "telephone" -> "☎️",
    "battery" -> "🔋",
    "plug" -> "🔌",
    "bulb" -> "💡",
    "flashlight" -> "🔦",
    "candle" -> "🕯️",
    "book" -> "📖",
    "books" -> "📚",
    "notebook" -> "📓",
    "scroll" -> "📜",
    "newspaper" -> "📰",
    "bookmark" -> "🔖",
    "label" -> "🏷️",
    "envelope" -> "✉️",
    "inboxTray" -> "📥",
    "outboxTray" -> "📤",
    "package" -> "📦",
    "mailbox" -> "📬",
    "pencil" -> "✏️",
    "pen" -> "🖊️",
    "paintbrush" -> "🖌️",
    "crayon" -> "🖍️",
    "memo" -> "📝",
    "folder" -> "📁",
    "openFolder" -> "📂",
    "file" -> "📄",
    "clipboard" -> "📋",
    "calendar" -> "📅",
    "pushpin" -> "📌",
    "paperclip" -> "📎",
    "ruler" -> "📏",
    "scissors" -> "✂️",
    "lock" -> "🔒",
    "unlock" -> "🔓",
    "key" -> "🔑",
    "hammer" -> "🔨",
    "axe" -> "🪓",
    "pick" -> "⛏️",
    "wrench" -> "🔧",
    "screwdriver" -> "🪛",
    "nutAndBolt" -> "🔩",
    "gear" -> "⚙️",
    "link" -> "🔗",
    "chain" -> "⛓️",
    "magnet" -> "🧲",
    "bomb" -> "💣",
    "dynamite" -> "🧨",
    "knife" -> "🔪",
    "dagger" -> "🗡️",
    "shield" -> "🛡️",
    "smoking" -> "🚬",
    "coffin" -> "⚰️",
    "microscope" -> "🔬",
    "telescope" -> "🔭",
    "crystal_ball" -> "🔮",
    "pill" -> "💊",
    "syringe" -> "💉",
    "stethoscope" -> "🩺",
    "broom" -> "🧹",
    "basket" -> "🧺",
    "soap" -> "🧼",
    "toilet" -> "🚽",
    "bell" -> "🔔",
    "bellOff" -> "🔕",
    "megaphone" -> "📣",
    "loudspeaker" -> "📢",
    "horn" -> "📯"
  )

  // --- Symbols & Signs ---
  val emojiSymbols: Map[String, String] = Map(
    "checkMark" -> "✅",
    "crossMark" -> "❌",
    "crossMarkCurly" -> "❎",
    "plus" -> "➕",
    "minus" -> "➖",
    "divide" -> "➗",
    "multiply" -> "✖️",
    "infinity" -> "♾️",
    "exclamation" -> "❗",
    "doubleExclamation" -> "‼️",
    "question" -> "❓",
    "whiteQuestion" -> "❔",
    "whiteExclamation" -> "❕",
    "hundred" -> "💯",
    "sparkle" -> "❇️",
    "sparkles" -> "✨",
    "star8" -> "✴️",
    "bang" -> "💥",
    "collision" -> "💥",
    "sweatDrops" -> "💦",
    "dash" -> "💨",
    "dizzySymbol" -> "💫",
    "speechBubble" -> "💬",
    "thoughtBubble" -> "💭",
    "zzz" -> "💤",
    "warning" -> "⚠️",
    "info" -> "ℹ️",
    "noEntry" -> "⛔",
    "prohibited" -> "🚫",
    "radioactive" -> "☢️",
    "biohazard" -> "☣️",
    "arrowUp" -> "⬆️",
    "arrowDown" -> "⬇️",
    "arrowLeft" -> "⬅️",
    "arrowRight" -> "➡️",
    "arrowUpRight" -> "↗️",
    "arrowDownRight" -> "↘️",
    "arrowDownLeft" -> "↙️",
    "arrowUpLeft" -> "↖️",
    "arrowUpDown" -> "↕️",
    "arrowLeftRight" -> "↔️",
    "arrowReturn" -> "↩️",
    "arrowCycle" -> "🔄",
    "recycle" -> "♻️",
    "trident" -> "🔱",
    "fleurDeLis" -> "⚜️",
    "copyright" -> "©️",
    "registered" -> "®️",
    "trademark" -> "™️"
  )

  // --- Activities & Sports ---
  val emojiActivities: Map[String, String] = Map(
    "soccerBall" -> "⚽",
    "basketball" -> "🏀",
    "football" -> "🏈",
    "baseball" -> "⚾",
    "tennis" -> "🎾",
    "volleyball" -> "🏐",
    "rugby" -> "🏉",
    "pool8ball" -> "🎱",
    "pingPong" -> "🏓",
    "badminton" -> "🏸",
    "boxing" -> "🥊",
    "goal" -> "🥅",
    "golf" -> "⛳",
    "fishing" -> "🎣",
    "skiing" -> "⛷️",
    "sled" -> "🛷",
    "iceSkate" -> "⛸️",
    "trophy" -> "🏆",
    "medal1st" -> "🥇",
    "medal2nd" -> "🥈",
    "medal3rd" -> "🥉",
    "medal" -> "🏅",
    "ticket" -> "🎫",
    "circus" -> "🎪",
    "art" -> "🎨",
    "movie" -> "🎬",
    "microphone" -> "🎤",
    "headphones" -> "🎧",
    "musicalNote" -> "🎵",
    "musicalNotes" -> "🎶",
    "guitar" -> "🎸",
    "trumpet" -> "🎺",
    "violin" -> "🎻",
    "drum" -> "🥁",
    "dice" -> "🎲",
    "chessPawn" -> "♟️",
    "bowling" -> "🎳",
    "gamePad" -> "🎮",
    "puzzle" -> "🧩",
    "joystick" -> "🕹️"
  )

  // --- Celebration & Events ---
  val emojiCelebration: Map[String, String] = Map(
    "party" -> "🎉",
    "confetti" -> "🎊",
    "balloon" -> "🎈",
    "birthdayCake" -> "🎂",
    "gift" -> "🎁",
    "ribbon" -> "🎀",
    "christmasTree" -> "🎄",
    "fireworks" -> "🎆",
    "sparkler" -> "🎇",
    "firecracker" -> "🧨",
    "jackOLantern" -> "🎃",
    "santa" -> "🎅",
    "pinata" -> "🪅"
  )

  // --- Clocks ---
  val emojiClocks: Map[String, String] = Map(
    "clock1" -> "🕐",
    "clock130" -> "🕜",
    "clock2" -> "🕑",
    "clock230" -> "🕝",
    "clock3" -> "🕒",
    "clock330" -> "🕞",
    "clock4" -> "🕓",
    "clock430" -> "🕟",
    "clock5" -> "🕔",
    "clock530" -> "🕠",
    "clock6" -> "🕕",
    "clock630" -> "🕡",
    "clock7" -> "🕖",
    "clock730" -> "🕢",
    "clock8" -> "🕗",
    "clock830" -> "🕣",
    "clock9" -> "🕘",
    "clock930" -> "🕤",
    "clock10" -> "🕙",
    "clock1030" -> "🕥",
    "clock11" -> "🕚",
    "clock1130" -> "🕦",
    "clock12" -> "🕛",
    "clock1230" -> "🕧",
    "hourglass" -> "⌛",
    "hourglassFlow" -> "⏳",
    "stopwatch" -> "⏱️",
    "timer" -> "⏲️",
    "alarm" -> "⏰"
  )

  // --- Flags (selection) ---
  val emojiFlags: Map[String, String] = Map(
    "checkeredFlag" -> "🏁",
    "triangularFlag" -> "🚩",
    "crossedFlags" -> "🎌",
    "blackFlag" -> "🏴",
    "whiteFlag" -> "🏳️",
    "rainbowFlag" -> "🏳️‍🌈",
    "pirateFlag" -> "🏴‍☠️"
  )

  // --- Developer / Tech ---
  val emojiDev: Map[String, String] = Map(
    "rocket" -> "🚀",
    "fire" -> "🔥",
    "bug" -> "🐛",
    "wrench" -> "🔧",
    "hammer" -> "🔨",
    "gear" -> "⚙️",
    "link" -> "🔗",
    "package" -> "📦",
    "label" -> "🏷️",
    "bookmark" -> "🔖",
    "magnifyLeft" -> "🔍",
    "magnifyRight" -> "🔎",
    "lightbulb" -> "💡",
    "memo" -> "📝",
    "clipboard" -> "📋",
    "pushpin" -> "📌",
    "lock" -> "🔒",
    "unlock" -> "🔓",
    "key" -> "🔑",
    "shield" -> "🛡️",
    "warning" -> "⚠️",
    "noEntry" -> "⛔",
    "checkMark" -> "✅",
    "crossMark" -> "❌",
    "sparkles" -> "✨",
    "star" -> "⭐",
    "zap" -> "⚡",
    "boom" -> "💥",
    "eyes" -> "👀",
    "brain" -> "🧠",
    "robot" -> "🤖",
    "construction" -> "🚧",
    "recycle" -> "♻️",
    "wastebasket" -> "🗑️",
    "inbox" -> "📥",
    "outbox" -> "📤",
    "satellite" -> "📡",
    "globe" -> "🌐",
    "floppy" -> "💾",
    "cd" -> "💿",
    "dvd" -> "📀",
    "laptop" -> "💻",
    "desktop" -> "🖥️",
    "phone" -> "📱",
    "electric_plug" -> "🔌",
    "battery" -> "🔋",
    "microscope" -> "🔬",
    "telescope" -> "🔭",
    "testTube" -> "🧪",
    "petriDish" -> "🧫",
    "dna" -> "🧬",
    "abacus" -> "🧮",
    "scroll" -> "📜",
    "triangularRuler" -> "📐",
    "straightRuler" -> "📏"
  )

  /** All emoji combined */
  val emoji: Map[String, String] =
    emojiFaces.map { case (k, v) => s"faces.$k" -> v } ++
      emojiHands.map { case (k, v) => s"hands.$k" -> v } ++
      emojiHearts.map { case (k, v) => s"hearts.$k" -> v } ++
      emojiAnimals.map { case (k, v) => s"animals.$k" -> v } ++
      emojiFood.map { case (k, v) => s"food.$k" -> v } ++
      emojiNature.map { case (k, v) => s"nature.$k" -> v } ++
      emojiTravel.map { case (k, v) => s"travel.$k" -> v } ++
      emojiObjects.map { case (k, v) => s"objects.$k" -> v } ++
      emojiSymbols.map { case (k, v) => s"symbols.$k" -> v } ++
      emojiActivities.map { case (k, v) => s"activities.$k" -> v } ++
      emojiCelebration.map { case (k, v) => s"celebration.$k" -> v } ++
      emojiClocks.map { case (k, v) => s"clocks.$k" -> v } ++
      emojiFlags.map { case (k, v) => s"flags.$k" -> v } ++
      emojiDev.map { case (k, v) => s"dev.$k" -> v }

  /** All single-char symbols combined into one map */
  val all: Map[String, Char] =
    quadrants.map { case (k, v) => s"quadrants.$k" -> v } ++
      braillePixels.map { case (k, v) => s"braillePixels.$k" -> v } ++
      boxThin.map { case (k, v) => s"boxThin.$k" -> v } ++
      boxHeavy.map { case (k, v) => s"boxHeavy.$k" -> v } ++
      boxDouble.map { case (k, v) => s"boxDouble.$k" -> v } ++
      boxRounded.map { case (k, v) => s"boxRounded.$k" -> v } ++
      boxDashed.map { case (k, v) => s"boxDashed.$k" -> v } ++
      boxExtra.map { case (k, v) => s"boxExtra.$k" -> v } ++
      blocks.map { case (k, v) => s"blocks.$k" -> v } ++
      arrows.map { case (k, v) => s"arrows.$k" -> v } ++
      geometric.map { case (k, v) => s"geometric.$k" -> v } ++
      checks.map { case (k, v) => s"checks.$k" -> v } ++
      braille.map { case (k, v) => s"braille.$k" -> v } ++
      keyboard.map { case (k, v) => s"keyboard.$k" -> v } ++
      cards.map { case (k, v) => s"cards.$k" -> v } ++
      checkboxes.map { case (k, v) => s"checkboxes.$k" -> v } ++
      misc.map { case (k, v) => s"misc.$k" -> v } ++
      miscExtra.map { case (k, v) => s"miscExtra.$k" -> v } ++
      math.map { case (k, v) => s"math.$k" -> v } ++
      music.map { case (k, v) => s"music.$k" -> v } ++
      zodiac.map { case (k, v) => s"zodiac.$k" -> v } ++
      currency.map { case (k, v) => s"currency.$k" -> v } ++
      superscript.map { case (k, v) => s"superscript.$k" -> v } ++
      subscript.map { case (k, v) => s"subscript.$k" -> v } ++
      fractions.map { case (k, v) => s"fractions.$k" -> v } ++
      roman.map { case (k, v) => s"roman.$k" -> v } ++
      circledNumbers.map { case (k, v) => s"circledNumbers.$k" -> v } ++
      circledNumbersFilled.map { case (k, v) =>
        s"circledNumbersFilled.$k" -> v
      } ++
      circledLetters.map { case (k, v) => s"circledLetters.$k" -> v } ++
      dingbats.map { case (k, v) => s"dingbats.$k" -> v } ++
      dingbatArrows.map { case (k, v) => s"dingbatArrows.$k" -> v } ++
      typography.map { case (k, v) => s"typography.$k" -> v } ++
      greekUpper.map { case (k, v) => s"greekUpper.$k" -> v } ++
      greekLower.map { case (k, v) => s"greekLower.$k" -> v } ++
      powerline.map { case (k, v) => s"powerline.$k" -> v } ++
      circles.map { case (k, v) => s"circles.$k" -> v } ++
      technical.map { case (k, v) => s"technical.$k" -> v } ++
      cjkBrackets.map { case (k, v) => s"cjkBrackets.$k" -> v }
}

