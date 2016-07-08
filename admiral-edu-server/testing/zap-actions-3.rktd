((200 (#"GET" #"https://www.captainteach.org/2166-dev/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/assignments/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/feedback/a1-577be86f/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/next/a1-577be86f/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/submit/a1-577be86f/tests/")
  #"Content-Type: multipart/form-data; boundary=---------------------------1216689572562349870338734643"
  #"-----------------------------1216689572562349870338734643\r\nContent-Disposition: form-data; name=\"file\"; filename=\"e\"\r\nContent-Type: application/octet-stream\r\n\r\nI amjbclements.\n\r\n-----------------------------1216689572562349870338734643--\r\n")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/next/a1-577be86f/"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/browse/a1-577be86f/tests/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/submit/a1-577be86f/tests/")
  #"Content-Type: application/x-www-form-urlencoded"
  #"action=submit")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/feedback/a1-577be86f/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/<HASH1>/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>"))
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
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>/<FILE1|0>"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH1>/<FILE1|0>/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/review/<HASH1>/tests/save")
  #"Content-Type: application/json; charset=UTF-8"
  #"{\"rubric\":[{\"class\":\"BasicElement\",\"prompt\":\"Click on the line number to add inline comments to the code to indicate missing tests, or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You must add a summative comment at the end.\",\"id\":\"prompt\"},{\"class\":\"LikertElement\",\"prompt\":\"These tests are complete, correct, and easy to read.\",\"id\":\"correctness\",\"minLabel\":\"Disagree\",\"maxLabel\":\"Agree\",\"rangeSize\":9,\"selected\":5}]}")
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/submit/<HASH1>/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/feedback/a1-577be86f/"))
 ;; artificially inserting review of second thingy:
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
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<FILE2|0>"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/file-container/<HASH2>/<FILE2|0>/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/review/<HASH2>/tests/save")
  #"Content-Type: application/json; charset=UTF-8"
  #"{\"rubric\":[{\"class\":\"BasicElement\",\"prompt\":\"Click on the line number to add inline comments to the code to indicate missing tests, or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You must add a summative comment at the end.\",\"id\":\"prompt\"},{\"class\":\"LikertElement\",\"prompt\":\"These tests are complete, correct, and easy to read.\",\"id\":\"correctness\",\"minLabel\":\"Disagree\",\"maxLabel\":\"Agree\",\"rangeSize\":9,\"selected\":5}]}")
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/review/submit/<HASH2>/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/feedback/a1-577be86f/"))


 )
