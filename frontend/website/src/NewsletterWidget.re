module Styles = {
  open Css;

  let container =
    merge([
      Style.Body.basic_semibold,
      style([position(`relative), fontWeight(`normal)]),
    ]);

  let successMessage =
    style([
      display(`flex),
      alignItems(`center),
      justifyContent(`center),
      position(`absolute),
      bottom(`zero),
      left(`zero),
      height(px(40)),
      width(px(400)),
      background(white),
      border(px(1), `solid, Style.Colors.jungle),
      color(Style.Colors.jungle),
      borderRadius(px(4)),
      visibility(`hidden),
      opacity(0.),
      transition("all", ~duration=150),
    ]);

  let textField =
    style([
      display(`inlineFlex),
      alignItems(`center),
      height(px(40)),
      borderRadius(px(4)),
      width(px(272)),
      color(Style.Colors.teal),
      padding(px(12)),
      border(px(1), `solid, Style.Colors.hyperlinkAlpha(0.3)),
      active([
        outline(px(0), `solid, `transparent),
        borderColor(Style.Colors.hyperlinkAlpha(0.7)),
      ]),
      focus([
        outline(px(0), `solid, `transparent),
        borderColor(Style.Colors.hyperlinkAlpha(0.7)),
      ]),
    ]);

  let submit =
    style([
      display(`inlineFlex),
      alignItems(`center),
      justifyContent(`center),
      color(white),
      backgroundColor(Style.Colors.jungle),
      border(px(0), `solid, `transparent),
      marginLeft(px(8)),
      height(px(40)),
      width(px(120)),
      borderRadius(px(4)),
      cursor(`pointer),
      active([outline(px(0), `solid, `transparent)]),
      focus([outline(px(0), `solid, `transparent)]),
      disabled([backgroundColor(Style.Colors.slateAlpha(0.3))]),
    ]);
};

let component =
  ReasonReact.statelessComponent("HeroSection.NewsletterWidget");
let make = _ => {
  ...component,
  render: _self =>
    <form id="newsletter-subscribe" className=Styles.container>
      <div className=Css.(style([marginBottom(px(8))]))>
        {ReasonReact.string("Subscribe to our newsletter for updates")}
      </div>
      <div id="success-message" className=Styles.successMessage>
        {ReasonReact.string({js|✓ Check your email|js})}
      </div>
      <input
        type_="email"
        name="email"
        placeholder="janedoe@example.com"
        className=Styles.textField
      />
      <input
        type_="submit"
        value="Subscribe"
        id="subscribe-button"
        className=Styles.submit
      />
      <RunScript>
        {js|
            function newsletterSubmit(e) {
              e.preventDefault();
              const formElement = document.getElementById('newsletter-subscribe');
              const request = new XMLHttpRequest();
              const submitButton = document.getElementById('subscribe-button');
              submitButton.setAttribute('disabled', 'disabled');
              request.onload = function () {
                const successMessage = document.getElementById('success-message');
                successMessage.style.visibility = "visible";
                successMessage.style.opacity = 1;
                setTimeout(function () {
                  submitButton.removeAttribute('disabled');
                  successMessage.style.visibility = "hidden";
                  successMessage.style.opacity = 0;
                }, 5000);
              };
              request.open("POST", "https://jfs501bgik.execute-api.us-east-2.amazonaws.com/dev/subscribe");
              request.send(new URLSearchParams(new FormData(formElement)));
              return false;
            }

            const formElement = document.getElementById('newsletter-subscribe');
            formElement.onsubmit = newsletterSubmit;
          |js}
      </RunScript>
    </form>,
};
();
