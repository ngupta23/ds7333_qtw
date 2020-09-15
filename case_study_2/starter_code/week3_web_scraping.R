library(XML)
ubase = "http://www.cherryblossom.org/"

#### From text
menURLs = 
  c("cb99m.htm", 
    "cb003m.htm", 
    "results/2001/oof_m.html",
    "results/2002/oofm.htm", 
    "results/2003/CB03-M.HTM",
    "results/2004/men.htm", 
    "results/2005/CB05-M.htm", 
    "results/2006/men.htm", 
    "results/2007/men.htm", 
    "results/2008/men.htm", 
    "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")
####

#### Text URLS
urls = paste(ubase, menURLs, sep="")
urls[1:4]

# 1999: http://www.cherryblossom.org/cb99m.htm
# 2000: http://www.cherryblossom.org/cb003m.htm
# 2001: http://www.cherryblossom.org/results/2001/oof_m.html

#### Textbook Function
extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    #added encoding for windows users who get an "A" symbol
    doc = htmlParse(url)    
    #doc = htmlParse(url, encoding="UTF-8")
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
#      els = strsplit(txt, "\r\n")[[1]]
      els2 = strsplit(txt, "\n")[[1]]
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

# Skip over the first pass
#### Individual Input Components for Testing: 1999
url <- 'http://www.cherryblossom.org/results/1999/cb99m.html'
year <- 1999
sex <- "male"
file <- NULL
####

# Skip over the first pass
#### Individual Input Components for Testing: 2000
url <- 'http://www.cherryblossom.org/results/2000/Cb003m.htm'
year <- 2000
sex <- "male"
file <- NULL
####

#### Textbook example with (1) URL
df1 <- extractResTable(url = "http://www.cherryblossom.org/results/2000/Cb003m.htm", year = 2000, sex = "male", file = NULL)
df2 <- extractResTable(url = "http://www.cherryblossom.org/results/1999/cb99m.html", year = 1999, sex = "male", file = NULL)
#df3 <- extractResTableV2(url = "http://www.cherryblossom.org/results/1999/cb99m.html", year = 1999, sex = "male", file = NULL)

#### Textbook extraction of Male tables (results in an error)
years = 1999:2012
menTables = mapply(extractResTable, url = urls, year = years)
#names(menTables) = years # can't run b/c menTables hasn't been created
#sapply(menTables, length) # can't run b/c menTables hasn't been created

menTables <- list()
for(i in 1:length(years)){
  print(years[i])
  menTables[[i]] <- try(extractResTable(url=urls[i], year=years[i]))
}

# Let's go check out the first two URLs
urls[1] # [1] "http://www.cherryblossom.org/cb99m.htm"
urls[2] # [1] "http://www.cherryblossom.org/cb003m.htm"

#### Revised URLS
menURLsV2 = 
  c("results/1999/cb99m.html", #"cb99m.htm"
    "results/2000/Cb003m.htm", #"cb003m.htm"
    "results/2001/oof_m.html", #"results/2001/oof_m.html"
    "results/2002/oofm.htm", #"results/2002/oofm.htm"
    "results/2003/CB03-M.HTM", #"results/2003/CB03-M.HTM"
    "results/2004/men.htm", #"results/2004/men.htm"
    "results/2005/CB05-M.htm", #"results/2005/CB05-M.htm"
    "results/2006/men.htm", #"results/2006/men.htm"
    "results/2007/men.htm", #"results/2007/men.htm"
    "results/2008/men.htm", #"results/2008/men.htm"
    "results/2009/09cucb-M.htm", #"results/2009/09cucb-M.htm"
    "results/2010/2010cucb10m-m.htm", #"results/2010/2010cucb10m-m.htm"
    "results/2011/2011cucb10m-m.htm", #"results/2011/2011cucb10m-m.htm"
    "results/2012/2012cucb10m-m.htm" #"results/2012/2012cucb10m-m.htm"
  )
####

#### Revised URLS
urlsV2 = paste(ubase, menURLsV2, sep="")
urlsV2[1:4]

#### Modified textbook extraction of Male tables (results in 1999 having (1) record)
menTables = mapply(extractResTable, url = urlsV2, year = years)
names(menTables) = years
sapply(menTables, length)

#### Code to compare and contrast the format of two different years
substr(menTables$'1999', start = 1, stop = 100)
substr(menTables$'2000', start = 1, stop = 100)
menTables$'2000'[1:10]

#### Revised Function
extractResTableV2 =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    #added encoding for windows users who get an "A" symbol
    doc = htmlParse(url, encoding="UTF-8")
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else if (year == 1999 & sex == "male") { # have to add this else if statement
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]   
    } 
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

#### Corrected function to pull down Male tables with consistent format
menTablesV2 = mapply(extractResTableV2, url = urlsV2, year = years)
names(menTablesV2) = years
sapply(menTablesV2, length)

#### Confirmation that the 1999 and other years have consistent formatting
menTablesV2$'1999'[1:10]
menTablesV2[[2]][1:10]

#### Save the outputs
save(menTablesV2, file = "CBMenTextTables.rda")

#### Now we need to investigate the differences between the male and female result pages
# 2000
df_male_2000 <- extractResTableV2(url = "http://www.cherryblossom.org/results/2000/Cb003m.htm", year = 2000, sex = "male", file = NULL)
df_female_2000 <- extractResTableV2(url = "http://www.cherryblossom.org/results/2000/Cb003f.htm", year = 2000, sex = "female", file = NULL)

df_female_2000[1:10]
df_male_2000[1:10]

# 2006
df_male_2006 <- extractResTableV2(url = "http://www.cherryblossom.org/results/2006/men.htm", year = 2006, sex = "male", file = NULL)
df_female_2006 <- extractResTableV2(url = "http://www.cherryblossom.org/results/2006/women.htm", year = 2006, sex = "female", file = NULL)

df_female_2006[1:10]
df_male_2006[1:10]

######################################################################
# Miscellaneous Code

menTables <- list()
for(i in 1:length(years)){
  menTables[[i]] <- try(extractResTable(url=urlsV2[i], year=years[i]))
}

# Breaking down the extractResTableV2 for 1999 - Men
url <- urlsV2[1]
doc = htmlParse(url, encoding="UTF-8")
pres = getNodeSet(doc, "//pre")
txt = xmlValue(pres[[1]])
els = strsplit(txt, "\r\n")[[1]]
els = strsplit(txt, "\n")[[1]]

# Breaking down the extractResTableV2 for 2009 - Men
url <- urlsV2[11]
doc = htmlParse(url, encoding="UTF-8")
div1 = getNodeSet(doc, "//div[@class='Section1']")
pres = getNodeSet(div1[[1]], "//pre")
els = sapply(pres, xmlValue)

