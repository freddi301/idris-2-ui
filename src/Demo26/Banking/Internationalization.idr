module Demo26.Banking.Internationalization

import Demo26.UI.View

public export
record Localization where
  constructor MakeLocalization
  navTabAccountsLabel : String
  navTabTransactionsLabel : String
  navTabSettingsLabel : String
  settingsSectionLanguage : String
  settingsLanguageEnglish : String
  settingsLanguageItalian : String
  settingsSectionTheme : String
  settingsThemeLight : String
  settingsThemeDark : String
  accountsTableHeaderAccount : String
  accountsTableHeaderBalance : String
  transactionsTableHeaderFrom : String
  transactionsTableHeaderTo : String
  transactionsTableHeaderAmount : String

English : Localization
English = MakeLocalization {
  navTabAccountsLabel = "Accounts",
  navTabTransactionsLabel = "Transactions",
  navTabSettingsLabel = "Settings",
  settingsSectionLanguage = "Language",
  settingsLanguageEnglish = "English",
  settingsLanguageItalian = "Italian",
  settingsSectionTheme = "Theme",
  settingsThemeLight = "Light",
  settingsThemeDark = "Dark",
  accountsTableHeaderAccount = "Account",
  accountsTableHeaderBalance = "Balance",
  transactionsTableHeaderFrom = "From",
  transactionsTableHeaderTo = "To",
  transactionsTableHeaderAmount = "Amount"
}

Italian : Localization
Italian = MakeLocalization {
  navTabAccountsLabel = "Conti",
  navTabTransactionsLabel = "Transazioni",
  navTabSettingsLabel = "Impostazioni",
  settingsSectionLanguage = "Lingua",
  settingsLanguageEnglish = "Inglese",
  settingsLanguageItalian = "Italiano",
  settingsSectionTheme = "Tema",
  settingsThemeLight = "Chiaro",
  settingsThemeDark = "Scuro",
  accountsTableHeaderAccount = "Conto",
  accountsTableHeaderBalance = "Saldo",
  transactionsTableHeaderFrom = "Da",
  transactionsTableHeaderTo = "A",
  transactionsTableHeaderAmount = "Importo"
}

namespace Language

  public export
  data Language = English | Italian

export
Eq Language where
  (==) English English = True
  (==) Italian Italian = True
  (==) _ _ = False

localization : Language -> Localization
localization English = English
localization Italian = Italian

LanguageContext = createContext Language
SetLanguageContext = createContext (Language -> StateUpdate)

export
provideLocalization : View -> View
provideLocalization child = do
  (themeName, setInternationalizationName) <- useState $ the Language English
  Provider LanguageContext themeName $ Provider SetLanguageContext setInternationalizationName $
  child

export
useLanguage : Exposed (Language, Language -> StateUpdate)
useLanguage = Expose $ \expose => do
  name <- LanguageContext
  setName <- SetLanguageContext
  expose (name, setName)

export
useI18n : Exposed Localization
useI18n = Expose $ \expose => do
  name <- LanguageContext
  expose $ localization name