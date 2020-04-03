module Footer = {
  module Link = {
    let footerStyle =
      Css.(
        style([
          Theme.Typeface.ibmplexsans,
          color(Theme.Colors.slate),
          textDecoration(`none),
          display(`inline),
          hover([color(Theme.Colors.hyperlink)]),
          fontSize(`rem(1.0)),
          fontWeight(`light),
          lineHeight(`rem(1.56)),
        ])
      );
    [@react.component]
    let make = (~last=false, ~link, ~name, ~children) => {
      <li className=Css.(style([display(`inline)]))>
        <a
          href=link
          className=footerStyle
          name={"footer-" ++ name}
          target="_blank">
          children
        </a>
        {last
           ? React.null
           : <span className=footerStyle ariaHidden=true>
               {React.string({js| · |js})}
             </span>}
      </li>;
    };
  };

  [@react.component]
  let make = (~bgcolor) => {
    <footer className=Css.(style([backgroundColor(bgcolor)]))>
      <section
        className=Css.(
          style(
            [
              maxWidth(`rem(96.0)),
              marginLeft(`auto),
              marginRight(`auto),
              // Not using Theme.paddingY here because we need the background
              // color the same (so can't use margin), but we also need some
              // top spacing.
              paddingTop(`rem(4.75)),
              paddingBottom(`rem(2.)),
            ]
            @ Theme.paddingX(`rem(4.0)),
          )
        )>
        <div
          className=Css.(
            style([
              display(`flex),
              justifyContent(`center),
              textAlign(`center),
              marginBottom(`rem(2.0)),
            ])
          )>
          <ul
            className=Css.(
              style([listStyleType(`none), ...Theme.paddingX(`zero)])
            )>
            <Link link="mailto:contact@o1labs.org" name="mail">
              {React.string("contact@o1labs.org")}
            </Link>
            <Link link="https://o1labs.org" name="o1www">
              {React.string("o1labs.org")}
            </Link>
            <Link link="https://twitter.com/codaprotocol" name="twitter">
              {React.string("Twitter")}
            </Link>
            <Link link="https://github.com/CodaProtocol/coda" name="github">
              {React.string("GitHub")}
            </Link>
            <Link link="https://codawiki.com" name="wiki">
              {React.string("Community Wiki")}
            </Link>
            <Link link="https://forums.codaprotocol.com" name="discourse">
              {React.string("Discourse")}
            </Link>
            <Link link="https://reddit.com/r/coda" name="reddit">
              {React.string("Reddit")}
            </Link>
            <Link link="https://t.me/codaprotocol" name="telegram">
              {React.string("Telegram")}
            </Link>
            <Link link="/tos" name="tos">
              {React.string("Terms of service")}
            </Link>
            <Link link="/privacy" name="privacy">
              {React.string("Privacy Policy")}
            </Link>
            <Link link="/jobs" name="hiring">
              {React.string("We're Hiring")}
            </Link>
            <Link
              link="https://s3.us-east-2.amazonaws.com/static.o1test.net/presskit.zip"
              name="presskit"
              last=true>
              {React.string("Press Kit")}
            </Link>
          </ul>
        </div>
        <p
          className=Css.(
            merge([
              Theme.Body.small,
              style([textAlign(`center), color(Theme.Colors.saville)]),
            ])
          )>
          {React.string({j|© 2020 O(1) Labs|j})}
        </p>
      </section>
    </footer>;
  };
};
let siteDescription = "Coda is the first cryptocurrency with a succinct blockchain. Our lightweight blockchain means anyone can use Coda directly from any device, in less data than a few tweets.";

[@react.component]
let make =
    (
      ~title,
      ~description=siteDescription,
      ~image="/static/img/coda_facebook_OG.jpg",
      ~route=?,
      ~children,
      ~footerColor=Theme.Colors.white,
    ) => {
  let router = Next.Router.useRouter();
  let route = Option.value(route, ~default=router.route);

  <>
    <Next.Head>
      <title> {React.string(title)} </title>
      <meta property="og:title" content=title />
      <meta property="og:image" content=image />
      <meta property="og:type" content="website" />
      <meta property="og:description" content=description />
      <meta name="description" content=description />
      <meta property="og:url" content={"https://codaprotocol.com" ++ route} />
      <link rel="canonical" href={"https://codaprotocol.com" ++ route} />
      <link
        rel="icon"
        type_="image/png"
        href="/static/favicon-32x32.png"
        sizes="32x32"
      />
      <link
        rel="icon"
        type_="image/png"
        href="/static/favicon-16x16.png"
        sizes="16x16"
      />
      <link
        href="https://cdn.jsdelivr.net/npm/@ibm/plex@4.0.2/css/ibm-plex.min.css"
        rel="stylesheet"
      />
      <link href="https://use.typekit.net/mta7mwm.css" rel="stylesheet" />
      // On recent versions of firefox, the browser will do a "flash of
      // unstyled content" for images by displaying the alt text(!) before the
      // image loads. Of course, we must disable this.
      <style>
        {React.string("img:-moz-loading { visibility: hidden; }")}
      </style>
    </Next.Head>
    <Nav />
    <div> children </div>
    <Footer bgcolor=footerColor />
    <CookieWarning />
  </>;
};
