module Styles = {
  open Css;
  let announcementBanner =
    merge([
      Theme.Type.announcement,
      style([
        color(Theme.Colors.digitalBlack),
        display(`flex),
        justifyContent(`spaceBetween),
        alignItems(`center),
        width(`percent(100.)),
        important(backgroundSize(`cover)),
        backgroundImage(`url("/static/img/AnnouncementBanner.png")),
        padding2(~v=`rem(0.5), ~h=`rem(0.5)),
      ]),
    ]);

  let flexCenter = style([display(`flex), alignItems(`center)]);

  let hideIfMobileElseShow =
    style([
      display(`none),
      media(Theme.MediaQuery.notMobile, [display(`flex)]),
    ]);

  let orangeText =
    style([
      color(Theme.Colors.orange),
      lineHeight(`rem(1.)),
      fontWeight(`num(550)),
    ]);

  let changeRegionSection =
    merge([flexCenter, style([paddingRight(`rem(1.))])]);

  let announcementText = style([paddingLeft(`rem(2.))]);

  let link = merge([flexCenter, style([cursor(`pointer)])]);

  let learnMoreText =
    merge([
      orangeText,
      style([
        marginLeft(`rem(0.4)),
        marginRight(`rem(0.2)),
        cursor(`pointer),
      ]),
    ]);

  let changeRegionText =
    merge([
      orangeText,
      hideIfMobileElseShow,
      style([paddingLeft(`rem(0.5)), paddingRight(`rem(0.5))]),
    ]);
};

[@react.component]
let make = (~children) => {
  <div className=Styles.announcementBanner>
    <div className=Styles.flexCenter>
      <span className=Styles.announcementText> children </span>
      <Next.Link href="/">
        <div className=Styles.link>
          <span className=Styles.learnMoreText>
            {React.string("Learn More")}
          </span>
          <Icon kind=Icon.ArrowRightMedium currentColor="orange" />
        </div>
      </Next.Link>
    </div>
    <div className=Styles.changeRegionSection>
      <Icon kind=Icon.World />
      <span
        className=Css.(
          merge([Styles.flexCenter, Styles.hideIfMobileElseShow])
        )>
        <span className=Styles.changeRegionText>
          {React.string("Change Region")}
        </span>
        <Icon kind=Icon.ChevronDown />
      </span>
    </div>
  </div>;
};
