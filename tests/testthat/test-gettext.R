test_that("gettext(), gettextf() and ngettext() behave like their base equivalent", {
  # Update .po and .mo files (only test in the source package, not R CMD check)
  if (file.exists("../../DESCRIPTION")) {# This is the source of the package
    cat("\nCompiling .po files...\n")
    tools::update_pkg_po("../..")
  }

  # Test with English language first
  old_lang <- Sys.setLanguage("en")

  expect_identical(
    gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc"),
    base::gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svMisc"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc"),
    base::ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc"),
    base::ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )

  # Test with French translations
  Sys.setLanguage("fr")

  expect_identical(
    gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc"),
    base::gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svMisc"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc"),
    base::ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc"),
    base::ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )

  Sys.setLanguage(old_lang)
})

test_that("gettext(), gettextf() and ngettext() can use lang=", {
  # Make sure R is in English language
  old_lang <- Sys.setLanguage("en")

  # Getting English messages, using lang= argument
  expect_identical(
    gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc", lang = "en"),
    base::gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svMisc", lang = "en"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc", lang = "en"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc", lang = "en"),
    base::ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc", lang = "en"),
    base::ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )

  # Test with French translations when R is in English language
  # First, get French translation from regular base functions
  Sys.setLanguage("fr")
  gettext_fr <- base::gettext("Test of svMisc's `gettext()` and `gettextf()`:",
    "This should be transtlated, if '%s' language is supported.",
    domain = "R-svMisc")
  gettextf_fr <- base::gettextf("This is message number %i", 1L,
    domain = "R-svMisc")
  ngettext_fr0 <- base::ngettext(0,
    "You asked for only one item", "You asked for several items",
    domain = "R-svMisc")
  ngettext_fr1 <- base::ngettext(1,
    "You asked for only one item", "You asked for several items",
    domain = "R-svMisc")
  ngettext_fr2 <- base::ngettext(2,
    "You asked for only one item", "You asked for several items",
    domain = "R-svMisc")

  # Switch R back to English
  Sys.setLanguage("en")

  expect_identical(
    gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc", lang = "fr"), gettext_fr
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svMisc", lang = "fr"), gettextf_fr
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc", lang = "fr"), ngettext_fr0
  )
  expect_identical(
    ngettext(1,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc", lang = "fr"), ngettext_fr1
  )
  expect_identical(
    ngettext(2,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc", lang = "fr"), ngettext_fr2
  )

  # Test an non-existing language
  expect_identical(
    gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc", lang = "xx"),
    base::gettext("Test of svMisc's `gettext()` and `gettextf()`:",
      "This should be transtlated, if '%s' language is supported.",
      domain = "R-svMisc")
  )
  expect_identical(
    gettextf("This is message number %i", 1L,
      domain = "R-svMisc", lang = "xx"),
    base::gettextf("This is message number %i", 1L,
      domain = "R-svMisc")
  )
  expect_identical(
    ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc", lang = "xx"),
    base::ngettext(0,
      "You asked for only one item", "You asked for several items",
      domain = "R-svMisc")
  )

  Sys.setLanguage(old_lang)
})
