module Styles = {
  open Css;

  let main =
    style([
      position(`absolute),
      top(`zero),
      left(`zero),
      background(white),
      zIndex(100),
      display(`flex),
      flexDirection(`row),
      paddingTop(Theme.Spacing.headerHeight),
      paddingBottom(Theme.Spacing.footerHeight),
      height(`vh(100.)),
      width(`vw(100.)),
    ]);
  let fadeIn = keyframes([(0, [opacity(0.)]), (100, [opacity(1.)])]);
  let body =
    merge([
      Theme.Text.Body.regular,
      style([animation(fadeIn, ~duration=1050, ~iterationCount=`count(1))]),
    ]);
};

[@react.component]
let make = () => {
  let (showOnboarding, closeOnboarding) =
    React.useContext(OnboardingProvider.context);
  let (onboardingStep, setOnboardingStep) = React.useState(() => 0);
  let prevStep = () => {
    setOnboardingStep(currentStep => currentStep - 1);
  };

  let nextStep = () => {
    setOnboardingStep(currentStep => currentStep + 1);
  };

  let onboardingSteps = [
    <WelcomeStep nextStep />,
    <SetupNodeStep nextStep prevStep />,
    <AccountCreationStep nextStep prevStep />,
    <CompletionStep closeOnboarding prevStep />,
  ];
  showOnboarding
    ? <div className=Styles.main>
        <OnboardingHeader />
        {Array.of_list(onboardingSteps)[onboardingStep]}
        <OnboardingFooter onboardingSteps onboardingStep />
      </div>
    : React.null;
};
