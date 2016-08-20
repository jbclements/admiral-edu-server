((200 (#"GET" #"https://www.captainteach.org/2166-dev/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/feedback"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/assignments/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/feedback/a1-577be86f/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/next/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/next"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/submit/a1-577be86f/tests/")
  #"Content-Type: multipart/form-data; boundary=---------------------------17163124721063370400707402466"
  #"-----------------------------17163124721063370400707402466\r\nContent-Disposition: form-data; name=\"file\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n-----------------------------17163124721063370400707402466--\r\n")
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/submit"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/submit/a1-577be86f"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/submit/a1-577be86f/tests/")
  #"Content-Type: multipart/form-data; boundary=---------------------------8437762701513248749522893022"
  #"-----------------------------8437762701513248749522893022\r\nContent-Disposition: form-data; name=\"file\"; filename=\"c\"\r\nContent-Type: application/octet-stream\r\n\r\natho\noth\ni am yokeleast\n\r\n-----------------------------8437762701513248749522893022--\r\n")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/next/a1-577be86f/"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/browse"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/browse-download/a1-577be86f/tests/c"))
 (#f
  (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests/download"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests/c"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/submit/a1-577be86f/tests/")
  #"Content-Type: multipart/form-data; boundary=---------------------------112131088517030417701360981174"
  #"-----------------------------112131088517030417701360981174\r\nContent-Disposition: form-data; name=\"file\"; filename=\"d\"\r\nContent-Type: application/octet-stream\r\n\r\nonth\ni am y.oht.\n\nwhat!\n\r\n-----------------------------112131088517030417701360981174--\r\n")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/next/a1-577be86f/"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/submit/a1-577be86f/tests/")
  #"Content-Type: application/x-www-form-urlencoded"
  #"action=submit")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/feedback/a1-577be86f/"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests/"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests/d"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/browse-download/a1-577be86f/tests/d"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/<HASH1>/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/review"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/file-container"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/review/<HASH1>/tests/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/<HASH1>"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/<HASH1>/tests"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>/download/<FILE1|0>"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>/download"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/submit/<HASH1>/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/submit/<HASH1>/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/<HASH2>/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/review/<HASH2>/tests/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/<HASH2>"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/<HASH2>/tests"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<FILE2|0>"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<FILE2|0>/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<HASH2|0>"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<HASH2|0>/save")
  #"Content-Type: application/json; charset=UTF-8"
  #"{\n\t\"comments\" :\n\t{\n\t\t\"1\" : \"asthnth!!!\n\"\n\t}\n}")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<HASH2|0>/save")
  #"Content-Type: application/json; charset=UTF-8"
  #"{\n\t\"comments\" :\n\t{\n\t\t\"1\" : \"asthnth!!!\n<i>a\"\n\t}\n}")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<HASH2|0>/save")
  #"Content-Type: application/json; charset=UTF-8"
  #"{\n\t\"comments\" :\n\t{\n\t\t\"1\" : \"asthnth!!!\n<i>abcd</i>\"\n\t}\n}")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/review/<HASH2>/tests/save")
  #"Content-Type: application/json; charset=UTF-8"
  #"{\"rubric\":[{\"class\":\"BasicElement\",\"prompt\":\"Click on the line number to add inline comments to the code to indicate missing tests, or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You must add a summative comment at the end.\",\"id\":\"prompt\"},{\"class\":\"LikertElement\",\"prompt\":\"These tests are complete, correct, and easy to read.\",\"id\":\"correctness\",\"minLabel\":\"Disagree\",\"maxLabel\":\"Agree\",\"rangeSize\":9,\"selected\":1}]}")
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/submit/<HASH2>/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/feedback/a1-577be86f/")))
