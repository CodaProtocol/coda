module Colors = {
  let string =
    fun
    | `rgb(r, g, b) => Printf.sprintf("rgb(%d,%d,%d)", r, g, b)
    | `rgba(r, g, b, a) => Printf.sprintf("rgba(%d,%d,%d,%f)", r, g, b, a)
    | `hsl(h, s, l) => Printf.sprintf("hsl(%d,%d%%,%d%%)", h, s, l)
    | `hsla(`deg(h), `percent(s), `percent(l), `num(a)) =>
      Printf.sprintf("hsla(%f,%f%%,%f%%,%f)", h, s, l, a);

  let fadedBlue = `rgb((111, 167, 197));
  let babyBlue = `hex("F4F8FB");

  let white = Css.white;
  let whiteAlpha = a => `rgba((255, 255, 255, a));
  let hyperlinkAlpha = a =>
    `hsla((`deg(201.), `percent(71.), `percent(52.), `num(a)));
  let hyperlink = hyperlinkAlpha(1.0);

  let hyperlinkHover = `hex("0AA9FF");
  let hyperlinkLight = `hsl((`deg(201.), `percent(71.), `percent(70.)));

  let metallicBlue = `rgb((70, 99, 131));
  let denimTwo = `rgb((61, 88, 120));
  let greyBlue = `rgb((118, 147, 190));
  let darkGreyBlue = `rgb((61, 88, 120));
  let greyishBrown = `rgb((74, 74, 74));

  let bluishGreen = `rgb((22, 168, 85));
  let offWhite = `rgb((243, 243, 243));
  let grey = `rgb((129, 146, 168));

  let azureAlpha = a => `rgba((45, 158, 219, a));
  let gandalf = `rgb((243, 243, 243));
  let veryLightGrey = `rgb((235, 235, 235));

  let slateAlpha = a =>
    `hsla((`deg(209.), `percent(20.), `percent(40.), `num(a)));
  let slate = slateAlpha(1.0);

  let navy = `rgb((0, 49, 90));
  let navyBlue = `rgb((0, 23, 74));
  let navyBlueAlpha = a => `rgba((0, 23, 74, a));
  let greyishAlpha = a => `rgba((170, 170, 170, a));
  let saville = `hsl((`deg(212.), `percent(33.), `percent(35.)));

  let clover = `rgb((22, 168, 85));
  let lightClover = `rgba((118, 205, 135, 0.12));

  let kernelAlpha = a => `rgba((0, 212, 0, a));
  let kernel = kernelAlpha(1.);

  let tealAlpha = a => `rgba((71, 130, 160, a));
  let teal = tealAlpha(1.);
  let tealBlueAlpha = a => `rgba((0, 170, 170, a));
  let tealBlue = tealBlueAlpha(1.);

  let rosebud = `rgb((163, 83, 111));
  let rosebudAlpha = a => `rgba((163, 83, 111, a));

  let blueBlue = `rgb((42, 81, 224));
  let midnight = `rgb((31, 45, 61));

  let india = `rgb((242, 183, 5));
  let indiaAlpha = a => `rgba((242, 183, 5, a));

  let amber = `rgb((242, 149, 68));
  let amberAlpha = a => `rgba((242, 149, 68, a));

  let marine = `rgb((51, 104, 151));
  let marineAlpha = a => `rgba((51, 104, 151, a));

  let jungleAlpha = a => `rgba((47, 172, 70, a));
  let jungle = jungleAlpha(1.);

  let tan = `hex("F1EFEA");
};

module Typeface = {
  open Css;

  let ibmplexserif = fontFamily(`custom("IBM Plex Serif, serif"));

  let ibmplexsans =
    fontFamily(`custom("IBM Plex Sans, Helvetica Neue, Arial, sans-serif"));

  let ibmplexmono = fontFamily(`custom("IBM Plex Mono, Menlo, monospace"));

  let aktivgrotesk = fontFamily(`custom("aktiv-grotesk-extended, sans-serif"));

  let rubik = fontFamily(`custom("Rubik, sans-serif"));

  let pragmataPro = fontFamily(`custom("PragmataPro, monospace"));

  let ddinexp = fontFamily(`custom("D-Din-Exp,Helvetica Neue, Arial, sans-serif"));
};

module MediaQuery = {
  let veryVeryLarge = "(min-width: 77rem)";
  let veryLarge = "(min-width: 70.8125rem)";
  let somewhatLarge = "(min-width: 65.5rem)";
  let tablet = "(min-width:60rem)";
  let desktop = "(min-width:105rem)";
  let full = "(min-width: 54rem)";
  let notMobile = "(min-width: 32rem)";
  let notSmallMobile = "(min-width: 25rem)";
  let statusLiftAlways = "(min-width: 38rem)";
  let statusLift = keepAnnouncementBar =>
    keepAnnouncementBar ? statusLiftAlways : "(min-width: 0rem)";

  // to adjust root font size (therefore pixels)
  let iphoneSEorSmaller = "(max-width: 374px)";
};

/** sets both paddingLeft and paddingRight, as one should */
let paddingX = m => Css.[paddingLeft(m), paddingRight(m)];

/** sets both paddingTop and paddingBottom, as one should */
let paddingY = m => Css.[paddingTop(m), paddingBottom(m)];

let generateStyles = rules => (Css.style(rules), rules);

module Link = {
  open Css;

  let (init, basicStylesNoHover) =
    generateStyles([
      Typeface.ibmplexsans,
      color(Colors.hyperlink),
      textDecoration(`none),
      fontWeight(`medium),
      fontSize(`rem(1.125)),
      letterSpacing(`rem(-0.0125)),
      lineHeight(`rem(1.5)),
      media(MediaQuery.notMobile, [fontSize(`rem(1.0))]),
    ]);

  module No_hover = {
    let basic = init;
  };

  let (basic, basicStyles) =
    generateStyles([
      hover([color(Colors.hyperlinkHover)]),
      ...basicStylesNoHover,
    ]);
};

module H1 = {
  open Css;

  let (hero, heroStyles) =
    generateStyles([
      Typeface.ibmplexsans,
      fontWeight(`num(200)),
      fontSize(`rem(2.25)),
      letterSpacing(`rem(-0.02375)),
      lineHeight(`rem(3.0)),
      color(Colors.saville),
      media(
        MediaQuery.full,
        [
          fontSize(`rem(3.0)),
          letterSpacing(`rem(-0.03125)),
          lineHeight(`rem(4.0)),
        ],
      ),
    ]);
};

module H2 = {
  open Css;

  let (basic, basicStyles) =
    generateStyles([
      Typeface.ibmplexsans,
      fontWeight(`normal),
      fontSize(`rem(2.25)),
      letterSpacing(`rem(-0.03125)),
      lineHeight(`rem(3.0)),
    ]);
};

module Technical = {
  open Css;
  let border = f => style([f(`px(3), `dashed, Colors.greyishAlpha(0.5))]);

  let basic =
    style([
      Typeface.pragmataPro,
      fontWeight(`normal),
      color(Css.white),
      fontSize(`rem(0.9375)),
      textTransform(`uppercase),
    ]);
};

module H3 = {
  open Css;

  let (basic, basicStyles) =
    generateStyles([
      Typeface.ibmplexsans,
      fontSize(`rem(1.25)),
      textAlign(`center),
      lineHeight(`rem(1.5)),
    ]);

  let wideNoColor =
    style([
      whiteSpace(`nowrap),
      fontSize(`rem(1.0)),
      letterSpacing(`em(0.25)),
      Typeface.aktivgrotesk,
      fontWeight(`medium),
      fontStyle(`normal),
      textAlign(`center),
      textTransform(`uppercase),
    ]);

  let wide = merge([wideNoColor, style([color(Colors.fadedBlue)])]);

  let wings = {
    let wing = [
      contentRule(`none),
      fontSize(`px(5)),
      verticalAlign(`top),
      lineHeight(`rem(1.3)),
      borderTop(`pt(1), `solid, `rgba((155, 155, 155, 0.3))),
      borderBottom(`pt(1), `solid, `rgba((155, 155, 155, 0.3))),
      ...paddingX(`rem(1.5)),
    ];

    merge([
      wide,
      style([
        before([marginRight(`rem(1.0)), ...wing]),
        after([marginLeft(`rem(1.0)), ...wing]),
      ]),
    ]);
  };

  module Technical = {
    let basic =
      style([
        Typeface.pragmataPro,
        fontSize(`rem(0.9375)),
        fontWeight(`bold),
        letterSpacing(`px(1)),
        textTransform(`uppercase),
      ]);

    let title = merge([basic, style([color(Css.black)])]);

    let boxed =
      merge([
        basic,
        Technical.border(Css.border),
        style([
          color(Colors.white),
          lineHeight(`rem(1.5)),
          display(`inlineFlex),
          justifyContent(`center),
          alignItems(`center),
          minWidth(`rem(9.0625)),
          height(`rem(3.)),
          margin(`auto),
          whiteSpace(`nowrap),
          padding2(~v=`zero, ~h=`rem(1.)),
        ]),
      ]);
  };
};

module H4 = {
  open Css;

  let (basic, basicStyles) =
    generateStyles([
      Typeface.ibmplexsans,
      textAlign(`center),
      fontSize(`rem(1.0625)),
      lineHeight(`rem(1.5)),
      letterSpacing(`rem(0.25)),
      opacity(50.0),
      textTransform(`uppercase),
      fontWeight(`normal),
      color(Colors.greyishBrown),
    ]);

  let semiBold = merge([basic, style([fontWeight(`semiBold)])]);
  let header =
    style([
      Typeface.ibmplexsans,
      textAlign(`center),
      fontSize(`rem(1.5)),
      lineHeight(`rem(2.0)),
      fontWeight(`semiBold),
      color(Colors.saville),
    ]);

  let (wide, wideStyles) =
    generateStyles([
      fontSize(`rem(0.75)),
      letterSpacing(`rem(0.125)),
      Typeface.aktivgrotesk,
      fontWeight(`medium),
      fontStyle(`normal),
      textAlign(`center),
      textTransform(`uppercase),
      media(MediaQuery.notMobile, [whiteSpace(`nowrap)]),
    ]);
};

module H5 = {
  open Css;

  let init =
    style([
      Typeface.ibmplexsans,
      fontSize(`rem(0.9345)),
      letterSpacing(`rem(0.125)),
      fontWeight(`normal),
      color(Colors.slateAlpha(0.5)),
      textTransform(`uppercase),
    ]);

  let basic = merge([init, style([lineHeight(`rem(1.5))])]);

  let tight = merge([init, style([lineHeight(`rem(1.25))])]);
};

module Body = {
  open Css;

  module Technical = {
    let (basic, basicStyles) =
      generateStyles([
        Typeface.pragmataPro,
        color(Css.white),
        fontSize(`rem(1.)),
        lineHeight(`rem(1.25)),
        letterSpacing(`rem(0.00625)),
      ]);
  };

  let (basic, basicStyles) =
    generateStyles([
      Typeface.ibmplexsans,
      color(Colors.saville),
      fontSize(`rem(1.125)),
      lineHeight(`rem(1.625)),
      fontWeight(`normal),
      letterSpacing(`rem(0.016)),
      media(
        MediaQuery.notMobile,
        [
          fontSize(`rem(1.0)),
          lineHeight(`rem(1.5)),
          letterSpacing(`rem(0.01)),
        ],
      ),
    ]);

  let basic_semibold = merge([basic, style([fontWeight(`semiBold)])]);

  let big =
    style([
      Typeface.ibmplexsans,
      color(Colors.darkGreyBlue),
      fontSize(`rem(1.125)),
      lineHeight(`rem(1.875)),
    ]);

  let big_semibold = merge([big, style([fontWeight(`semiBold)])]);

  let small =
    style([
      Typeface.ibmplexsans,
      fontSize(`rem(0.8125)),
      opacity(0.5),
      lineHeight(`rem(1.25)),
    ]);

  let basic_small =
    style([
      Typeface.ibmplexsans,
      fontSize(`rem(0.8125)),
      color(Colors.saville),
    ]);
};

// Match Tachyons setting pretty much everything to border-box
Css.global(
  "a,article,aside,blockquote,body,code,dd,div,dl,dt,fieldset,figcaption,figure,footer,form,h1,h2,h3,h4,h5,h6,header,html,input[type=email],input[type=number],input[type=password],input[type=tel],input[type=text],input[type=url],legend,li,main,nav,ol,p,pre,section,table,td,textarea,th,tr,ul",
  [Css.boxSizing(`borderBox)],
);

// Reset padding that appears only on some browsers
Css.global(
  "h1,h2,h3,h4,h5,fieldset,ul,li,p,figure",
  Css.[
    unsafe("paddingInlineStart", "0"),
    unsafe("paddingInlineEnd", "0"),
    unsafe("paddingBlockStart", "0"),
    unsafe("paddingBlockEnd", "0"),
    unsafe("marginInlineStart", "0"),
    unsafe("marginInlineEnd", "0"),
    unsafe("marginBlockStart", "0"),
    unsafe("marginBlockEnd", "0"),
    unsafe("WebkitPaddingBefore", "0"),
    unsafe("WebkitPaddingStart", "0"),
    unsafe("WebkitPaddingEnd", "0"),
    unsafe("WebkitPaddingAfter", "0"),
    unsafe("WebkitMarginBefore", "0"),
    unsafe("WebkitMarginAfter", "0"),
  ],
);

Css.global("p", Css.[marginTop(`rem(1.)), marginBottom(`rem(1.))]);
