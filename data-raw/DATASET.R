# This script creates the internal data set.

library(dplyr)

# Institutions ----
# Isr Institution Regular Expressions.
# These are an (incomplete) list of Isr institutions and the regular expressions that can be used to match strings.
inst_regex <- readxl::read_excel(
  file.path("inst", "extdata", "institution_strings.xlsx"),
  sheet = "regex"
  )

# Web Scrape Institution String Replacements.
# This sheet contains the exact string matches to various institutions as scraped from bard-isus. Because the scraped data is so
inst_strings <- readxl::read_excel(
  file.path("inst", "extdata", "institution_strings.xlsx"),
  sheet = "string_replacements"
) %>%
  dplyr::mutate(string_match = stringr::str_squish(string_match)) %>%
  dplyr::mutate(conversion = stringr::str_squish(conversion))

# Wrong BARD's ----
# Phrases and REGEX for identifying easily confused funding sources.
# Points are assigned based on how important the match is overall. For instance, for the non-ISR BARD program in India, finding "Department of Energy" is good, but better if we find "India" as well. More to the point, some these strings, by themselves, don't definitively lock out other funding pools.
neg_funds <- tibble::tibble(
  name = c(
    "Bard College",
    "C. R. Bard, Inc.",
    "C. R. Bard, Inc.",
    "C. R. Bard, Inc.",
    "C. R. Bard, Inc.",
    "C. R. Bard, Inc.",
    "C. R. Bard, Inc.",
    "Biomolecular Assembly, Recognition and Dynamics",
    "Biomolecular Assembly, Recognition and Dynamics",
    "Biomolecular Assembly, Recognition and Dynamics",
    "Biomolecular Assembly, Recognition and Dynamics",
    "Biomolecular Assembly, Recognition and Dynamics",
    "Commission of Higher Education, Philippines",
    "N-BARD at Hiroshima University",
    "The Francis and Catherine Bard Professor Rush University Medical Center",
    "Irving Bard Memorial Fellowship (NIH?)",
    "Bard Access Systems (Salt Lake City, Utah)",
    "Bard Richmond Fellowship",
    "Demathieu Bard Group",
    "Bard Benelux NV",
    "Swedish Bard of Fisheries",
    "Medical",
    "Bard Baukol Fellowship (UND)"
  ),
  regex = c(
    # Bard College (University)
    "bard\\s(early\\s)?(coll\\b|college|summer\\sresearch|graduate\\b\\s\\bcenter|\\-Rockefeller)",
    # C. R. Bard (Medical Technology)
    "(BD(\\/|\\s|\\-)BARD)|(BARD(\\/|\\s|\\-)BD)",
    "C\\.?\\s?R\\.?\\s?BARD",
    "Bard(\\sPeripheral)?\\sVascular",
    "\\bBARD[^;]{0,4}(Peripheral|Davol|Medical|Karlsruhe|(Covington\\,\\sGeorgia)|Electrophysiology|Angiomed|Murray\\sHill|Norden|WOCN|Corporation|Interventional|(Endo)?Urolog(y|ical)|Pharma|Medica|Limited|Biopsy|compan(y|ies)|Inc)\\b",
    "\\bBard\\b\\s\\bInternational",
    "\\bLutonix\\b",
    # Biomolecular Assembly, Recognition and Dynamics
    "\\bIndia\\b",
    "Department\\sof\\sAtomic\\sEnergy",
    "\\bSaha\\b\\s\\bInstitute\\b",
    "\\bbiomolecular\\b\\s\\bassembly\\b\\,?\\s\\brecognition\\b\\s((and|&)\\s)?\\bdynamics\\b",
    "\\bSIN\\-5\\.04",  # grant number
    # Commission of Higher Education, Philippines
    "(\\bCHED\\b(\\-|\\s)\\bBARD\\b)",
    "\\bN\\s?\\-BARD\\b",
    "\\bBard\\b\\s\\bEndowed\\b\\s\\bChair",
    "Irving\\b\\s\\bBard\\b\\s\\bMemorial",
    "Bard\\b\\s\\bAccess\\b\\s\\bSystems",
    "Bard\\b\\s\\bRichmond",
    "Demathieu[^;]{1,10}Bard|Bard[^;]{1,10}Demathieu",
    "Bard\\b\\s\\bBenelux",
    "Swedish\\b\\s\\bBard[^;]{1,10}Fisheries",
    "(?<!(Howard\\sHughes))[^;]\\bMedical\\b",
    "\\bBard\\b[^;]{1,10}Baukol"
  ),
  points = c(
    # Bard College (University)
    1,
    # C. R. Bard (Medical Technology)
    1,
    1,
    1,
    0.7,
    0.5,
    0.7,
    # Biomolecular Assembly, Recognition and Dynamics
    0.05,
    0.70,
    0.20,
    1,
    1,
    # Commission of Higher Education, Philippines
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0.05,
    1
  ),
  notes = c(
    "Bard College, a liberal arts school in Red Hook, NY.",
    "Former medical company based in NJ, now owned by BD. Sometimes addressed as Bard Peripheral Vascular. It also has many divisions and acquisitions (eg. Angiomed).",
    as.character(NA),
    as.character(NA),
    as.character(NA),
    "I believe this is C. R. Bard, but it is unclear with a web search.",
    "There is a \"BARD\" project in India that appears to have nothing to do with the US-Israel BARD. Some keywords here include: the Saha Institute, CSIR, Department of Atomic Energy (DAB or DAE), India, etc.",
    as.character(NA),
    as.character(NA),
    as.character(NA),
    as.character(NA),
    as.character(NA),
    "There is only one record as of this writing, CHED-BARD.",
    "Natural Science Center for Basic Research and Development appears to have little to do with Israel.",
    "In my search, I didn't find any other endowed chairs (esp. any related to Israel), so this regex is a blanket search of Bard Endowed Chair",
    "I couldn't find much information, but this fellowship seems to be through NIH? Seems to fund a number of medical researchers.",
    "Medical company located in UT.",
    "A grant for medical researchers, unclear institution.",
    "Real Estate Company in EU.",
    "Belgian Medical company.",
    "Swedish thing.",
    "There are much fewer US-Isr BARD research on medicine than there are research from C.R. Bard on medical research. Most of the US-Isr BARD research on medicine is in association with Howard Hughes. Because of this fuzzy match, we only give a small penalty.",
    "History fellowship at U North Dakota."
  )
)

# Right Bard's ----
pos_funds <- tibble::tibble(
  name = c(
    "United States & Israel",
    "Israel",
    "Binational * Agricultural",
    "Binational * Research and Development",
    "Binational Agricultural * Development",
    "BARD Grant Number",
    "USDA/NIFA/NSERC BARD",
    "TDA/Vaadia",
    "BARD website",
    "Howard Hughes Medical Institute",
    "BARD and BSF",
    "NIFA BARD",
    "grant-style-1",
    "grant-style-2",
    "grant-style-3",
    "Binational Agricultural Research",
    "Bard Senior Fellowship",
    "Senior Fellowship grant-style-4"
    ),
  regex = c(
    # US Israel
    # the length of 0-8 between US and Israel is sort of arbitrary.
    "(((\\bU\\.?S\\.?A?\\b)|(United\\sStates))[^;]{0,8}(Israel))|((Israel)[^;]{0,8}((\\bU\\.?S\\.?A?\\b)|(United\\sStates)))",
    "(?<!\\bBeth\\b(\\s|\\-))\\bIsrael",
    # word separation by 30 to allow for " United States - Israel " (24 char)
    "Bi\\-?(national|lateral)\\s[^;]{0,30}Agricultur(e|al)",
    "Bi\\-?(national|lateral)\\s[^;]{0,30}Research\\s(((and)|&)\\s)?Development",
    "Bi\\-?(national|lateral)\\sAgricultur(e|al)[^;]{0,30}Development",
    # word separation by 30 letters to be generous
    "(?<!C\\.?R\\.?\\s)\\bBARD\\b[^;.,)]{0,30}\\[",
    "((USDA|NIFA|NSERC).*\\bBARD\\b)|(\\bBARD\\b.*(USDA|NIFA|NSERC))",
    "(Vaadia|TDA)\\-BARD",
    "\\bbard\\b\\-\\bisus\\b\\.",
    "(\\bHoward\\b\\s\\bHughes\\b)|\\bHHMI\\b",
    "(\\bBARD\\b[^;]{0,20}\\bBSF\\b)|(\\bBSF\\b[^;]{0,20}\\bBARD\\b)",
    "\\bNIFA\\b[^;]{0,20}\\bBARD\\b",
    "\\b(US|IS|NB)\\-\\d{4}\\-\\d{2}[[:alpha:]]{0,2}\\b",
    "\\b(MB|TB|CB|CA)\\-\\d{4}\\-\\d{2}[[:alpha:]]{0,2}\\b",
    "\\bBARD\\b\\s(\\[|\\()\\b(US|IS)(?![A-Za-z])-?\\d{4}",
    "\\bBinational\\b\\s\\bAgricultural\\b\\s\\bResearch",
    "\\bBARD\\b\\s\\bSenior\\b\\s\\bResearch\\b\\s\\bFellowship",
    "\\bBARD\\b\\s(\\[|\\()\\bFR(?![A-Za-z])-?\\d{2}"
  ),
  points = c(
    # US Israel
    0.8,
    0.2,
    # Binational
    0.6,
    0.4,
    0.2,
    # Grant opening, ie. BARD [...]
    0.6,
    # Accessory Organizations: NIFA, TDA, etc.
    0.5,
    1,
    1,
    0.5,
    0.7,
    0.7,
    # Grant styles eg. IS-9999-99
    0.7,
    0.5,
    0.2,
    0.4,
    # Bard Senior Fellowships
    0.7,
    0.4
  ),
  notes = c(
    "The relation between the US and Israel is being checked here. Since the US comes in so many forms, this regex is complicated. It should match for things like: US, U.S., USA, etc. It should not match words that end in 'us' like 'rapacious'. The rule also makes sure that Israel follows at some point after or vice versa. This code does not distinguish how close the two phrases are in terms of word separation.",
    "Just Israel is a good signal too.",
    "This one is a strong signal, but reduced score because the next regex is also trying to capture the same notion and will likely overlap.",
    as.character(NA),
    as.character(NA),
    "This tries to capture BARD and an open square bracket because this is what many of the grants use. It does make exceptions if there is a semicolon in the way.",
    "Since BARD is often given in relation to USDA or NIFA, we make this match worthwhile.",
    "Programs specific to BARD",
    "US-IS BARD website is www.bard-isus.org",
    "For whatever reason, every mention of BARD + Howard Hughes is US-Isr BARD. This is one easier way of finding 'medical research' that is Israeli adjacent. A quick search reveals an article in Haaretz about 6 Israeli researchers at HHMI.",
    "BARD and BSF are often found together; BSF is the Binational Science Fund.",
    "NIFA and BARD seem common companions, and empirically US-IS BARD.",
    "Captures most common BARD grant numbers",
    "Captures secondary BARD grant numbers",
    as.character(NA),
    as.character(NA),
    "Senior Fellowship",
    "Senior Fellowship"
  )
)

# Store data ----
usethis::use_data(
  inst_regex,
  inst_strings,
  neg_funds,
  pos_funds,
  internal = TRUE,
  overwrite = TRUE
)
