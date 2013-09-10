```shell
$ ghc -threaded IOStreamServer.hs +RTS -N4 -o server
$ ./IOStreamServer
$ curl http://localhost:3000 
<!doctype html>
<html>
  <head>

  </head>
    <body>
        <p>Index page</p>
          </body>
          </html>
$ ====== REQUEST ======
(Request {requestMethod = GET, requestUri = "/",
requestVersion = "1.1"},[Header {headerName = "User-Agent",
headerValue = ["curl/7.24.0 (x86_64-apple-darwin12.0)
libcurl/7.24.0 OpenSSL/0.9.8x zlib/1.2.5"]},Header
{headerName = "Host", headerValue = ["localhost:3000"]},Header
{headerName = "Accept", headerValue = ["*/*"]}])
====== RESPONSE =====
"HTTP/1.1 200 OK\r\nContent Length: 96\r\n\r\n<!doctype
html>\n<html>\n  <head>\n    \n  </head>\n  <body>\n    <p>Index
    page</p>\n  </body>\n</html>\n"
```
