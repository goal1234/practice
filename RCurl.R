
library(rvest)
#AUTH_ANY Constants for identifying Authentication Schemes

#base64 Encode/Decode base64 content
base64(txt, encode = !inherits(txt, "base64"), mode = "character")

# encode and then decode a simple string.
txt = "Some simple text for base 64 to handle"
x = base64(txt)
base64(x)
# encode to a raw vector
x = base64("Simple text", TRUE, "raw")
# decode to a character string.
ans = base64Decode(x)
ans == txt
# decoded to a raw format.
ans = base64Decode(x, "raw")
# Binary data
# f = paste(R.home(), "doc", "html", "logo.jpg", sep = .Platform$file.sep)
f = system.file("examples", "logo.jpg", package = "RCurl")
img = readBin(f, "raw", file.info(f)[1, "size"])
b64 = base64Encode(img, "raw")
back = base64Decode(b64, "raw")
identical(img, back)
# alternatively, we can encode to a string and then decode back again
# to raw and see that we preserve the date.
enc = base64Encode(img, "character")
dec = base64Decode(enc, "raw")
identical(img, dec)

# The following would be the sort of computation we could do if we
# could have in-memory raw connections.
# We would save() some objects to such an in-memory binary/raw connection
# and then encode the resulting raw vector into a character vector.
# Then we can insert that into a message, e.g. an email message or
# an XML document and when we receive it in a different R session
# we would get the string and reverse the encoding from the string to
# a raw vector
# In the absence of that in-memory connection facility in save(),
# we can use a file.
x = 1:10
# save two objects - a function and a vector
f = paste(tempfile(), "rda", sep = ".")
save(base64, x, file = f)
# now read the results back from that file as a raw vector
data = readBin(f, "raw", file.info(f)[1,"size"])
# base64 encode it
txt = base64Encode(data, "character")
if(require(XML)) {
  tt = xmlTree("r:data", namespaces = c(r = "http://www.r-project.org"))
  tt$addNode(newXMLTextNode(txt))
  out = saveXML(tt)
  doc = xmlRoot(xmlTreeParse(out, asText = TRUE))
  rda = base64Decode(xmlValue(doc), "raw")
  f = tempfile()
  writeBin(rda, f)
  e = new.env()
  load(f, e)
  objects(e)
}
# we'd like to be able to do
# con = rawConnection(raw(), 'r+')
# save(base64, x, file = con)
# txt = base64Encode(rawConnectionValue(con), "character")
# ... write and read xml stuff
# val = xmlValue(doc)
# rda = base64Decode(val, "raw")
# e = new.env()
# input = rawConnection(o, "r")
# load(input, e)

#---basicHeaderGatherer Functions for processing the response header of a libcurl request---
basicHeaderGatherer(txt = character(), max = NA)
parseHTTPHeader(lines, multi = TRUE)

if(url.exists("http://www.omegahat.net/RCurl/index.html")) {
  h = basicHeaderGatherer()
  getURI("http://www.omegahat.net/RCurl/index.html",
         headerfunction = h$update)
  names(h$value())
  h$value()
}

#---basicTextGatherer Cumulate text across callbacks (from an HTTP response---
basicTextGatherer(txt = character(), max = NA, value = NULL,
                  .mapUnicode = TRUE)
multiTextGatherer(uris, binary = rep(NA, length(uris)))
debugGatherer()

if(url.exists("http://www.omegahat.net/RCurl/index.html")) {
  txt = getURL("http://www.omegahat.net/RCurl/index.html", write = basicTextGatherer())
  h = basicTextGatherer()
  txt = getURL("http://www.omegahat.net/RCurl/index.html", write = h$update)
  # Cumulate across pages.
  txt = getURL("http://www.omegahat.net/index.html", write = h$update)
  headers = basicTextGatherer()
  txt = getURL("http://www.omegahat.net/RCurl/index.html",
               header = TRUE, headerfunction = headers$update)
  # Now read the headers.
  headers$value()
  headers$reset()
  # Debugging callback
  d = debugGatherer()
  x = getURL("http://www.omegahat.net/RCurl/index.html", debugfunction = d$update, verbose = TRUE)
  names(d$value())
  d$value()[["headerIn"]]
  uris = c("http://www.omegahat.net/RCurl/index.html",
           "http://www.omegahat.net/RCurl/philosophy.html")
  g = multiTextGatherer(uris)
  txt = getURIAsynchronous(uris, write = g)
  names(txt)
  nchar(txt)
  # Now don't use names for the gatherer elements.
  g = multiTextGatherer(length(uris))
  txt = getURIAsynchronous(uris, write = g)
  names(txt)
  nchar(txt)
}

## Not run:
Sys.setlocale(,"en_US.latin1")
Sys.setlocale(,"en_US.UTF-8")
uris = c("http://www.omegahat.net/RCurl/index.html",
         "http://www.omegahat.net/RCurl/philosophy.html")
g = multiTextGatherer(uris)
txt = getURIAsynchronous(uris, write = g)
## End(Not run)

#---binaryBuffer Create internal C-level data structure for collecting binary data---#
binaryBuffer(initialSize = 5000)

if(url.exists("http://www.omegahat.net/RCurl/xmlParse.html.gz")) {
  buf = binaryBuffer()
  # Now fetch the binary file.
  getURI("http://www.omegahat.net/RCurl/xmlParse.html.gz",
         write = getNativeSymbolInfo("R_curl_write_binary_data")$address,
         file = buf@ref)
  # Convert the internal data structure into an R raw vector
  b = as(buf, "raw")
  if(require(Rcompression))
    gunzip(b)
}

#---CFILE Create a C-level handle for a file---#
## Not run:
filename = system.file("tests", "amazon3.R", package = "RCurl")
f = CFILE(filename)
if(url.exists('http://s3.amazonaws.com/'))
  curlPerform(url = "http://s3.amazonaws.com/RRupload/duncan2",
              upload = TRUE,
              readdata = f@ref,
              infilesize = file.info(filename)[1, "size"])
## End(Not run)

#---chunkToLineReader Utility that collects data from the HTTP reply into lines and calls userprovided function.---
chunkToLineReader(f, verbose = FALSE)
#getURI and the write argument. getForm, postForm curlPerform

# Read a rectangular table of data into R from the URL
# and add up the values and the number of values read.
summer =
  function()
  {
    total = 0.0
    numValues = 0
    list(read = function(txt) {
      con = textConnection(txt)
      on.exit(close(con))
      els = scan(con)
      numValues <<- numValues + length(els)
      total <<- total + sum(els)
      ""
    },
    result = function() c(total = total, numValues = numValues))
  }
s = summer()
if(url.exists("http://www.omegahat.net/RCurl/matrix.data"))
  getURL("http://www.omegahat.net/RCurl/matrix.data", write = chunkToLineReader(s$read)$read)

#---clone Clone/duplicate an object---#
h = getCurlHandle(verbose = TRUE)
other = dupCurlHandle(h)
curlSetOpt(curl = h, verbose = FALSE)

#---complete Complete an asynchronous HTTP request---#
if(url.exists("http://eeyore.ucdavis.edu/cgi-bin/testForm1.pl")) {
  f = system.file("NAMESPACE", package = "RCurl")
  postForm("http://eeyore.ucdavis.edu/cgi-bin/testForm1.pl",
           "fileData" = fileUpload(f))
  postForm("http://eeyore.ucdavis.edu/cgi-bin/testForm1.pl",
           "fileData" = fileUpload("",
                                   paste(readLines(f), collapse = "\n"),
                                   "text/plain"))
  postForm("http://eeyore.ucdavis.edu/cgi-bin/testForm1.pl",
           "fileData" = fileUpload(f,
                                   paste(readLines(f), collapse = "\n")
           ),
           .opts = list(verbose = TRUE, header = TRUE))
}

#---CURLEnums Classes and coercion methods for enumerations in libcurl---#
#---curlError Raise a warning or error about a CURL problem---#
curlError(type, msg, asError = TRUE)

# This illustrates generating and catching an error.
# We intentionally give a mis-spelled URL.
tryCatch(curlPerform(url = "ftp.wcc.nrcs.usda.govx"),
         COULDNT_RESOLVE_HOST = function(x) cat("resolve problem\n"),
         error = function(x) cat(class(x), "got it\n"))

#curlEscape Handle characters in URL that need to be escaped
curlEscape(urls)
curlUnescape(urls)
curlPercentEncode(x, amp = TRUE, codes = PercentCodes, post.amp = FALSE)

curlEscape("http://www.abc.com?x=a is a sentence&a b=and another")
# Reverse it should get back original
curlUnescape(curlEscape("http://www.abc.com?x=a is a sentence&a b=and another"))

##CurlFeatureBits Constants for libcurl##
#curlGlobalInit Start and stop the Curl library
curlGlobalInit(flags = c("ssl", "win32"))
curlGlobalCleanup()
# Activate only the SSL.
curlGlobalInit("ssl")
## Not run:
# Don't run these automatically as we should only call this function
# once per R session
# Everything, the default.
curlGlobalInit()
# Nothing.
curlGlobalInit("none")
curlGlobalInit(0)
## End(Not run)

##CURLHandle-class Class "CURLHandle" for synchronous HTTP requests
tt = basicTextGatherer()
myOpts = curlOptions(verbose = TRUE, header = TRUE, writefunc = tt[[1]])
# note that the names are expanded, e.g. writefunc is now writefunction.
names(myOpts)
myOpts[["header"]]
myOpts[["header"]] <- FALSE
# Using the abbreviation "hea" is an error as it matches
# both
# myOpts[["hea"]] <- FALSE
# Remove the option from the list
myOpts[["header"]] <- NULL

#---curlPerform Perform the HTTP query---#
curlPerform(..., .opts = list(), curl = getCurlHandle(), .encoding = integer())
curlMultiPerform(curl, multiple = TRUE)
if(url.exists("http://www.omegahat.net/RCurl")) {
  h = basicTextGatherer()
  curlPerform(url = "http://www.omegahat.net/RCurl", writefunction = h$update)
  # Now read the text that was cumulated during the query response.
  h$value()
}
if(url.exists("http://services.soaplite.com/hibye.cgi")) {
  # SOAP request
  body = '<?xml version="1.0" encoding="UTF-8"?>\
<SOAP-ENV:Envelope SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" \
xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" \
xmlns:xsd="http://www.w3.org/1999/XMLSchema" \
xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" \
xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance">\
<SOAP-ENV:Body>\
<namesp1:hi xmlns:namesp1="http://www.soaplite.com/Demo"/>\
</SOAP-ENV:Body>\
</SOAP-ENV:Envelope>\n'
  h$reset()
  curlPerform(url = "http://services.soaplite.com/hibye.cgi",
              httpheader=c(Accept="text/xml", Accept="multipart/*",
                           SOAPAction='"http://www.soaplite.com/Demo#hi"',
                           'Content-Type' = "text/xml; charset=utf-8"),
              postfields=body,
              writefunction = h$update,
              verbose = TRUE
  )
  body = h$value()
}

# Using a C routine as the reader of the body of the response.
if(url.exists("http://www.omegahat.net/RCurl/index.html")) {
  routine = getNativeSymbolInfo("R_internalWriteTest", PACKAGE = "RCurl")$address
  curlPerform(URL = "http://www.omegahat.net/RCurl/index.html",
              writefunction = routine)
}

##---curlSetOpt Set values for the CURL options---#
curlSetOpt(..., .opts = list(), curl = getCurlHandle(),
           .encoding = integer(), .forceHeaderNames = FALSE,
           .isProtected = FALSE)
if(url.exists("http://www.omegahat.net")) {
  curl = getCurlHandle()
  # Note the header that extends across two lines with the second line
  # prefixed with white space.
  curlSetOpt( .opts = list(httpheader = c(Date = "Wed, 1/2/2000 10:01:01",
                                          foo="abc\n extra line"), verbose = TRUE),
              curl = curl)
  ans = getURL("http://www.omegahat.net", curl = curl)
}

#---curlVersion Information describing the Curl library---#
curlVersion(id = 0)
dynCurlReader(curl = getCurlHandle(), txt = character(), max = NA,
              value = NULL, verbose = FALSE, binary = NA, baseURL = NA,
              isHTTP = NA, encoding = NA)

# Each of these examples can be done with getURLContent().
# These are here just to illustrate the dynamic reader.
if(url.exists("http://www.omegahat.net/Rcartogram/demo.jpg")) {
  header = dynCurlReader()
  curlPerform(url = "http://www.omegahat.net/Rcartogram/demo.jpg",
              headerfunction = header$update, curl = header$curl())
  class( header$value() )
  length( header$value() )
}
if(url.exists("http://www.omegahat.net/dd.gz")) {
  # gzip example.
  header = dynCurlReader()
  curlPerform(url = "http://www.omegahat.net/dd.gz",
              headerfunction = header$update, curl = header$curl())
  class( header$value() )
  length( header$value() )
  if(require(Rcompression))
    gunzip(header$value())
}
# Character encoding example
## Not run:
header = dynCurlReader()
curlPerform(url = "http://www.razorvine.net/test/utf8form/formaccepter.sn",
            postfields = c(text = "ABC", outputencoding = "UTF-8"),
            verbose = TRUE,
            writefunction = header$update, curl = header$curl())
class( header$value() )
Encoding( header$value() )
## End(Not run)

#-------fileUpload Specify information about a file to upload in an HTTP request----#
fileUpload(filename = character(), contents = character(), contentType = character())
#----findHTTPHeaderEncoding Find the encoding of the HTTP response from the HTTP header---#
findHTTPHeaderEncoding("Content-Type: text/html;charset=ISO-8859-1\r\n")
findHTTPHeaderEncoding("Content-Type: text/html; charset=utf-8\r\n")

#ftpUpload Upload content via FTP
ftpUpload(what, to, asText = inherits(what, "AsIs") || is.raw(what),
          ..., curl = getCurlHandle())
## Not run:
ftpUpload(I("Some text to be uploaded into a file\nwith several lines"),
          "ftp://login:password@laptop17/ftp/zoe",
)
ftpUpload(I("Some text to be uploaded into a file\nwith several lines"),
          "ftp://laptop17/ftp/zoe",
          userpwd = "login:password"
)
ftpUpload(system.file("examples", "system.png", package = "RCurl"),
          "ftp://login:password@laptop17/ftp/Election.rda",
          postquote = c("CWD subdir", "RNFR Election.rda", "RNTO ElectionPolls.rda")
)
## End(Not run)

#---getBinaryURL Download binary content---#
getBinaryURL(url, ..., .opts = list(), curl = getCurlHandle(),
             .buf = binaryBuffer(.len), .len = 5000)
u = "http://www.omegahat.net/RCurl/data.gz"
if(url.exists(u)) {
  content = getBinaryURL(u)
  if(require(Rcompression)) {
    x = gunzip(content)
    read.csv(textConnection(x))
  } else {
    tmp = tempfile()
    write(content, file = tmp)
    read.csv(gzfile(tmp))
  }
  
  # Working from the Content-Type in the header of the HTTP response.
  h = basicTextGatherer()
  content = getBinaryURL(u, .opts = list(headerfunction = h$update))
  header = parseHTTPHeader(h$value())
  type = strsplit(header["Content-Type"], "/")[[1]]
  if(type[2] %in% c("x-gzip", "gzip")) {
    if(require(Rcompression))
      x = gunzip(content)
  }
}

#---getBitIndicators Operate on bit fields---#
getBitIndicators(val, defs)
setBitIndicators(vals, defs)
getBitIndicators(7, c(A = 1, B = 2, C = 4))
getBitIndicators(3, c(A = 1, B = 2, C = 4))
getBitIndicators(5, c(A = 1, B = 2, C = 4))

#---getCurlErrorClassNames Retrieve names of all curl error classes--#
getCurlHandle Create libcurl handles
getCurlHandle(..., .opts = NULL, .encoding = integer(),
              .defaults = getOption("RCurlOptions"))
dupCurlHandle(curl, ..., .opts = NULL, .encoding = integer())
getCurlMultiHandle(..., .handles = list(...))

options(RCurlOptions = list(verbose = TRUE,
                            followlocation = TRUE,
                            autoreferer = TRUE,
                            nosignal = TRUE))
if(url.exists("http://www.omegahat.net/RCurl")) {
  x = getURL("http://www.omegahat.net/RCurl")
  # here we override one of these.
  x = getURL("http://www.omegahat.net/RCurl", verbose = FALSE)
}

#---getCurlInfo Access information about a CURL request---#
getCurlInfo(curl, which = getCurlInfoConstants())
getCurlInfoConstants()

if(url.exists("http://www.omegahat.net/RCurl/index.html")) {
  curl = getCurlHandle()
  getURL("http://www.omegahat.net/RCurl/index.html", curl = curl)
  getCurlInfo(curl)
  rm(curl) # release the curl!
}

#---getFormParams Extract parameters from a form query string---#
getFormParams(query, isURL = grepl("^(http|\\?)", query))
if(url.exists("http://www.omegahat.net/foo/bob.R")) {
  getFormParams("http://www.omegahat.net/foo/bob.R?xyz=1&abc=verylong")
  getFormParams("xyz=1&abc=verylong")
  getFormParams("xyz=1&abc=&on=true")
  getFormParams("xyz=1&abc=")
}

#---getURIAsynchronous Download multiple URIs concurrently, with inter-leaved downloads---#
getURIAsynchronous(url, ..., .opts = list(), write = NULL,
                   curl = getCurlHandle(),
                   multiHandle = getCurlMultiHandle(), perform = Inf,
                   .encoding = integer(), binary = rep(NA, length(url)))
uris = c("http://www.omegahat.net/RCurl/index.html",
         "http://www.omegahat.net/RCurl/philosophy.xml")
txt = getURIAsynchronous(uris)
names(txt)
nchar(txt)


#---getURL Download a URI---#
getURL(url, ..., .opts = list(),
       write = basicTextGatherer(.mapUnicode = .mapUnicode),
       curl = getCurlHandle(), async = length(url) > 1,
       .encoding = integer(), .mapUnicode = TRUE)
getURI(url, ..., .opts = list(),
       write = basicTextGatherer(.mapUnicode = .mapUnicode),
       curl = getCurlHandle(), async = length(url) > 1,
       .encoding = integer(), .mapUnicode = TRUE)
getURLContent(url, ..., curl = getCurlHandle(.opts = .opts), .encoding = NA,
              binary = NA, .opts = list(...),
              header = dynCurlReader(curl, binary = binary,
                                     baseURL = url, isHTTP = isHTTP,
                                     encoding = .encoding),
              isHTTP = length(grep('^[[:space:]]*http', url)) > 0)

omegahatExists = url.exists("http://www.omegahat.net")
# Regular HTTP
if(omegahatExists) {
  txt = getURL("http://www.omegahat.net/RCurl/")
  # Then we could parse the result.
  if(require(XML))
    htmlTreeParse(txt, asText = TRUE)
}
# HTTPS. First check to see that we have support compiled into
# libcurl for ssl.
if(interactive() && ("ssl" %in% names(curlVersion()$features))
   && url.exists("https://sourceforge.net/")) {
  txt = tryCatch(getURL("https://sourceforge.net/"),
                 error = function(e) {
                   getURL("https://sourceforge.net/",
                          ssl.verifypeer = FALSE)
                 })
}
# Create a CURL handle that we will reuse.
if(interactive() && omegahatExists) {
  curl = getCurlHandle()
  pages = list()
  for(u in c("http://www.omegahat.net/RCurl/index.html",
             "http://www.omegahat.net/RGtk/index.html")) {
    pages[[u]] = getURL(u, curl = curl)
  }
}
# Set additional fields in the header of the HTTP request.
# verbose option allows us to see that they were included.
if(omegahatExists)
  getURL("http://www.omegahat.net", httpheader = c(Accept = "text/html",
                                                   MyField = "Duncan"),
         verbose = TRUE)
# Arrange to read the header of the response from the HTTP server as
# a separate "stream". Then we can break it into name-value
# pairs. (The first line is the HTTP/1.1 200 Ok or 301 Moved Permanently
# status line)
if(omegahatExists) {
  h = basicTextGatherer()
  txt = getURL("http://www.omegahat.net/RCurl/index.html",
               header= TRUE, headerfunction = h$update,
               httpheader = c(Accept="text/html", Test=1), verbose = TRUE)
  print(paste(h$value(NULL)[-1], collapse=""))
  read.dcf(textConnection(paste(h$value(NULL)[-1], collapse="")))
}
# Test the passwords.
if(omegahatExists) {
  x = getURL("http://www.omegahat.net/RCurl/testPassword/index.html", userpwd = "bob:duncantl")
  # Catch an error because no authorization
  # We catch the generic HTTPError, but we could catch the more specific "Unauthorized" error
  # type.
  x = tryCatch(getURLContent("http://www.omegahat.net/RCurl/testPassword/index.html"),
               HTTPError = function(e) {
                 cat("HTTP error: ", e$message, "\n")
               })
}

## Not run:
# Needs specific information from the cookie file on a per user basis
# with a registration to the NY times.
x = getURL("http://www.nytimes.com",
           header = TRUE, verbose = TRUE,
           cookiefile = "/home/duncan/Rcookies",
           netrc = TRUE,
           maxredirs = as.integer(20),
           netrc.file = "/home2/duncan/.netrc1",
           followlocation = TRUE)
## End(Not run)
if(interactive() && omegahatExists) {
  d = debugGatherer()
  x = getURL("http://www.omegahat.net", debugfunction = d$update, verbose = TRUE)
  d$value()
}
#############################################
# Using an option set in R
if(interactive() && omegahatExists) {
  opts = curlOptions(header = TRUE, userpwd = "bob:duncantl", netrc = TRUE)
  getURL("http://www.omegahat.net/RCurl/testPassword/index.html", verbose = TRUE, .opts = opts)
  # Using options in the CURL handle.
  h = getCurlHandle(header = TRUE, userpwd = "bob:duncantl", netrc = TRUE)
  getURL("http://www.omegahat.net/RCurl/testPassword/index.html", verbose = TRUE, curl = h)
}
# Use a C routine as the reader. Currently gives a warning.
if(interactive() && omegahatExists) {
  routine = getNativeSymbolInfo("R_internalWriteTest", PACKAGE = "RCurl")$address
  getURL("http://www.omegahat.net/RCurl/index.html", writefunction = routine)
}

# Example
if(interactive() && omegahatExists) {
  uris = c("http://www.omegahat.net/RCurl/index.html",
           "http://www.omegahat.net/RCurl/philosophy.xml")
  txt = getURI(uris)
  names(txt)
  nchar(txt)
  txt = getURI(uris, async = FALSE)
  names(txt)
  nchar(txt)
  routine = getNativeSymbolInfo("R_internalWriteTest", PACKAGE = "RCurl")$address
  txt = getURI(uris, write = routine, async = FALSE)
  names(txt)
  nchar(txt)
  # getURLContent() for text and binary
  x = getURLContent("http://www.omegahat.net/RCurl/index.html")
  class(x)
  x = getURLContent("http://www.omegahat.net/RCurl/data.gz")
  class(x)
  attr(x, "Content-Type")
  x = getURLContent("http://www.omegahat.net/Rcartogram/demo.jpg")
  class(x)
  attr(x, "Content-Type")
  curl = getCurlHandle()
  dd = getURLContent("http://www.omegahat.net/RJSONIO/RJSONIO.pdf",
                     curl = curl,
                     header = dynCurlReader(curl, binary = TRUE,
                                            value = function(x) {
                                              print(attributes(x))
                                              x}))
}
# FTP
# Download the files within a directory.
if(interactive() && url.exists('ftp://ftp.wcc.nrcs.usda.gov')) {
  url = 'ftp://ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/'
  filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  # Deal with newlines as \n or \r\n. (BDR)
  # Or alternatively, instruct libcurl to change \n's to \r\n's for us with crlf = TRUE
  # filenames = getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
  filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
  con = getCurlHandle( ftp.use.epsv = FALSE)
  # there is a slight possibility that some of the files that are
  # returned in the directory listing and in filenames will disappear
  # when we go back to get them. So we use a try() in the call getURL.
  contents = sapply(filenames[1:5], function(x) try(getURL(x, curl = con)))
  names(contents) = filenames[1:length(contents)]
}

##---guessMIMEType Infer the MIME type from a file name---##
guessMIMEType(name, default = NA)
guessMIMEType(c("foo.txt", "foo.png", "foo.jpeg", "foo.Z", "foo.R"))
guessMIMEType("foo.bob")
guessMIMEType("foo.bob", "application/x-binary")


##---httpPUT Simple high-level functions for HTTP PUT and DELETE---##
httpPUT(url, content, ..., curl = getCurlHandle())
httpPOST(url, ..., curl = getCurlHandle())
httpDELETE(url, ..., curl = getCurlHandle())
httpGET(url, ..., curl = getCurlHandle())
httpHEAD(url, ..., curl = getCurlHandle())
httpOPTIONS(url, ..., curl = getCurlHandle())
## Not run:
# create a database in a CouchDB server
httpPUT("http://127.0.0.1:5984/temp_db")
# Insert an entry into an ElasticSearch dabtabase.
httpPUT("http://localhost:9200/a/b/axyz", '{"abc" : 123}')
# Then delete the database
httpDELETE("http://127.0.0.1:5984/temp_db")
## End(Not run)

##---merge.list Method for merging two lists by name---##
merge.list(x, y, ...)

## Not run:
# Not exported.
merge.list(list(a=1, b = "xyz", c = function(x, y) {x+y}),
           list(a = 2, z = "a string"))
# No values in y
merge.list(list(a=1, b = "xyz", c = function(x, y) {x+y}), list())
# No values in x
merge.list(list(), list(a=1, b = "xyz", c = function(x, y) {x+y}))
## End(Not run)

##---mimeTypeExtensions Mapping from extension to MIME type---##
##---postForm Submit an HTML form---##
postForm(uri, ..., .params = list(), .opts = curlOptions(url = uri),
         curl = getCurlHandle(), style = 'HTTPPOST',
         .encoding = integer(), binary = NA, .checkParams = TRUE,
         .contentEncodeFun = curlEscape)
.postForm(curl, .opts, .params, style = 'HTTPPOST')
getForm(uri, ..., .params = character(), .opts = list(), curl = getCurlHandle(),
        .encoding = integer(), binary = NA, .checkParams = TRUE)

if(url.exists("http://www.google.com")) {
  # Two ways to submit a query to google. Searching for RCurl
  getURL("http://www.google.com/search?hl=en&lr=&ie=ISO-8859-1&q=RCurl&btnG=Search")
  # Here we let getForm do the hard work of combining the names and values.
  getForm("http://www.google.com/search", hl="en", lr="",
          ie="ISO-8859-1", q="RCurl", btnG="Search")
  # And here if we already have the parameters as a list/vector.
  getForm("http://www.google.com/search", .params = c(hl="en", lr="",
                                                      ie="ISO-8859-1", q="RCurl", btnG="Search"))
}
# Now looking at POST method for forms.
url <- "http://wwwx.cs.unc.edu/~jbs/aw-wwwp/docs/resources/perl/perl-cgi/programs/cgi_stdin.cgi"
if(url.exists(url))
  
  postForm(url,
           name = "Bob", "checkedbox" = "spinich",
           submitButton = "Now!",
           textarea = "Some text to send",
           selectitem = "The item",
           radiobutton = "a", style = "POST")
# Genetic database via the Web.
if(url.exists('http://www.wormbase.org/db/searches/advanced/dumper')) {
  x = postForm('http://www.wormbase.org/db/searches/advanced/dumper',
               species="briggsae",
               list="",
               flank3="0",
               flank5="0",
               feature="Gene Models",
               dump = "Plain TEXT",
               orientation = "Relative to feature",
               relative = "Chromsome",
               DNA ="flanking sequences only",
               .cgifields = paste(c("feature", "orientation", "DNA", "dump","relative"), collapse=", "))
  # Note that we don't have to paste multiple values together ourselves,
  # e.g. the .cgifields can be specified as a character vector rather
  # than a string.
  x = postForm('http://www.wormbase.org/db/searches/advanced/dumper',
               species="briggsae",
               list="",
               flank3="0",
               flank5="0",
               feature="Gene Models",
               dump = "Plain TEXT",
               orientation = "Relative to feature",
               relative = "Chromsome",
               DNA ="flanking sequences only",
               .cgifields =c("feature", "orientation", "DNA", "dump", "relative"))
}


##---RCurlInternal Internal functions---##
h = getCurlHandle()
curlSetOpt(customrequest = "DELETE")
reset(h)
scp(host, path, keypasswd = NA, user = getUserName(), rsa = TRUE,
    key = sprintf(c("~/.ssh/id_%s.pub", "~/.ssh/id_%s"),
                  if (rsa) "rsa" else "dsa"),
    binary = NA, size = 5000, curl = getCurlHandle(), ...)

## Not run:
x = scp("eeyore.ucdavis.edu", "/home/duncan/OmegaWeb/index.html",
        "My.SCP.Passphrase", binary = FALSE)
x = scp("eeyore.ucdavis.edu", "/home/duncan/OmegaWeb/RCurl/xmlParse.bz2",
        "My.SCP.Passphrase")
if(require(Rcompression))
  o = bunzip2(x)
## End(Not run)


##---url.exists Check if URL exists---##
url.exists(url, ..., .opts = list(...),
           curl = getCurlHandle(.opts = .opts),
           .header = FALSE)

url.exists("http://www.omegahat.net/RCurl")
try(url.exists("http://www.omegahat.net/RCurl-xxx"))

