[@react.component]
let make = () => {
  let settingsValue = AddressBookProvider.createContext();
  let (isOnboarding, _) as onboardingValue =
    OnboardingProvider.createContext();
  let dispatch = CodaProcess.useHook();
  let toastValue = ToastProvider.createContext();
  let (locale, _) = Locale.useLocale(); // TODO expose setLocale to children

  <AddressBookProvider value=settingsValue>
    <OnboardingProvider value=onboardingValue>
      <ProcessDispatchProvider value=dispatch>
        <ReasonApollo.Provider client=Apollo.client>
          <ReactIntl.IntlProvider
            locale={locale->Locale.toString}
            messages={locale->Locale.getTranslations->Locale.translationsToDict}>
            {isOnboarding
               ? <Onboarding />
               : <ToastProvider value=toastValue>
                   <Header />
                   <Main> <SideBar /> <Router /> </Main>
                   <Footer />
                 </ToastProvider>}
          </ReactIntl.IntlProvider>
        </ReasonApollo.Provider>
      </ProcessDispatchProvider>
    </OnboardingProvider>
  </AddressBookProvider>;
};