module Demo26.Banking.Screen

import Demo26.UI.View

import Demo26.Banking.Domain
import Demo26.Banking.Route
import Demo26.Banking.Theme
import Demo26.Banking.Component
import Demo26.Banking.Internationalization

%default total

export
Navigation : View
Navigation = do
  i18n <- useI18n
  Tabs [
    (Accounts, i18n.navTabAccountsLabel),
    (Transactions, i18n.navTabTransactionsLabel),
    (Settings, i18n.navTabSettingsLabel)
  ]

export
Screen : Route -> View

Screen Settings = do
  i18n <- useI18n
  (language, setLanguage) <- useLanguage
  (themeName, setThemeName) <- useThemeName
  Flex [
    Section i18n.settingsSectionLanguage,
    Radio {
      value = language,
      onChange = setLanguage,
      options = [
        (English, i18n.settingsLanguageEnglish),
        (Italian, i18n.settingsLanguageItalian)
      ]
    },
    Section i18n.settingsSectionTheme,
    Radio {
      value = themeName,
      onChange = setThemeName,
      options = [
        (Light, i18n.settingsThemeLight),
        (Dark, i18n.settingsThemeDark)
      ]
    }
  ]

Screen Accounts = do
  i18n <- useI18n
  Table (the (List (AccountId, Integer)) [(1, 10), (2, -10)]) [
    MakeColumn {
      header = i18n.accountsTableHeaderAccount,
      cell = \(accountId, _) => show accountId
    },
    MakeColumn {
      header = i18n.accountsTableHeaderBalance,
      cell = \(_, balance) => show balance
    }
  ] 

Screen Transactions = do
  i18n <- useI18n
  Table (the (List (AccountId, AccountId, Nat)) [
    (1, 2, 10),
    (2, 1, 5)
  ]) [
    MakeColumn {
      header = i18n.transactionsTableHeaderFrom,
      cell = \(from, _, _) => show from
    },
    MakeColumn {
      header = i18n.transactionsTableHeaderTo,
      cell = \(_, to, _) => show to
    },
    MakeColumn {
      header = i18n.transactionsTableHeaderAmount,
      cell = \(_, _, amount) => show amount
    }
  ]

Screen (Account accountId) = Text $ "Account"
