;; EDITED!
((200 (#"GET" #"https://www.captainteach.org/2166-dev/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/new-student/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/roster/")
  #"Content-Type: application/x-www-form-urlencoded"
  #"action=create-student&uid=yokeleast%40gmail.com")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/upload-roster/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/roster/")
  #"Content-Type: multipart/form-data; boundary=---------------------------8328410292607632571781420519"
  #"-----------------------------8328410292607632571781420519\r\nContent-Disposition: form-data; name=\"action\"\r\n\r\nprocess-roster\r\n-----------------------------8328410292607632571781420519\r\nContent-Disposition: form-data; name=\"file\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n-----------------------------8328410292607632571781420519--\r\n")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/upload-roster/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/roster/")
  #"Content-Type: multipart/form-data; boundary=---------------------------205091904268377093898143351"
  ;; edited to change email addresses...
  #"-----------------------------205091904268377093898143351\r\nContent-Disposition: form-data; name=\"action\"\r\n\r\nprocess-roster\r\n-----------------------------205091904268377093898143351\r\nContent-Disposition: form-data; name=\"file\"; filename=\"roster.txt\"\r\nContent-Type: text/plain\r\n\r\nfrogstar@example.com\nmf2@example.com\n\r\n-----------------------------205091904268377093898143351--\r\n")
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/johnbclements@gmail.com/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/new-student/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/roster/")
  #"Content-Type: application/x-www-form-urlencoded"
  #"action=create-student&uid=froggy%40example.com")
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/change-role/ta-role/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/change-role/instructor-role/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/change-role/student-role/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/roster/edit/froggy@example.com/drop/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/roster/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/assignments/"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/author/"))
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/author/validate")
  #"Content-Type: application/json; charset=UTF-8"
  #"nthont.h\n.h.h")
 (200
  (#"POST" #"https://www.captainteach.org/2166-dev/author/validate")
  #"Content-Type: application/json; charset=UTF-8"
  #"name: Assignment 1 Captain Teach\nid: a1-577be86f\ndescription: Problem 3.3.3 Solution\nsteps:\n  - id: tests\n    instructions: \"Submit your solution to problem 3.3.3\"\n    reviews:\n        - student-submission:\n            id: student-reviews\n            amount: 2\n            rubric:\n              - instruction: Click on the line number to add inline comments to the code to indicate missing tests, or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You must add a summative comment at the end.\n              - likert:\n                  id: correctness\n                  text: These tests are complete, correct, and easy to read.\n                  min-label: Disagree\n                  max-label: Agree\n                  granularity: 9\n")
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/assignments/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/dashboard/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/assignments"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/assignments/dashboard"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/author/edit/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/author/edit"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/author/edit/a1-577be86f/validate-save")
  #"Content-Type: application/json; charset=UTF-8"
  #"name: Assignment 1 Captain Teach\nid: a1-577be86f\ndescription: Problem 3.3.3 Solution\nsteps:\n  - id: tests\n    instructions: \"Submit your zzzZ solution to problem 3.3.3\"\n    reviews:\n        - student-submission:\n            id: student-reviews\n            amount: 2\n            rubric:\n              - instruction: Click on the line number to add inline comments to the code to indicate missing tests, or unclear or poorly organized code. Also, use comments to indicate particularly well-organized or clear tests. You must add a summative comment at the end.\n              - likert:\n                  id: correctness\n                  text: These tests are complete, correct, and easy to read.\n                  min-label: Disagree\n                  max-label: Agree\n                  granularity: 9\n")
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/author/edit/a1-577be86f"))
 (200 (#"GET" #"https://www.captainteach.org/2166-dev/assignments/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/dashboard/a1-577be86f/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/status/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/assignments/status"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/status/a1-577be86f/tests/"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/status/a1-577be86f"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/status/a1-577be86f/tests/student-reviews/"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/status/a1-577be86f/tests"))
 (200
  (#"GET" #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/dependencies"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests/student-reviews/"))
 (#f
  (#"GET" #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f"))
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests/student-reviews/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (#f
  (#"GET"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests/student-reviews"))
 (400
  (#"POST"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests/student-reviews/upload/")
  #"Content-Type: multipart/form-data; boundary=---------------------------13366093481714364216531286513"
  #"-----------------------------13366093481714364216531286513\r\nContent-Disposition: form-data; name=\"file-1\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n-----------------------------13366093481714364216531286513\r\nContent-Disposition: form-data; name=\"file-2\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n-----------------------------13366093481714364216531286513--\r\n")
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests/student-reviews/upload/")
  #"Content-Type: multipart/form-data; boundary=---------------------------11204644443618097651410515698"
  #"-----------------------------11204644443618097651410515698\r\nContent-Disposition: form-data; name=\"file-1\"; filename=\"b\"\r\nContent-Type: application/octet-stream\r\n\r\nhh\nhh\nhh\n\r\n-----------------------------11204644443618097651410515698\r\nContent-Disposition: form-data; name=\"file-2\"; filename=\"a\"\r\nContent-Type: application/octet-stream\r\n\r\naoenuh\n\r\n-----------------------------11204644443618097651410515698--\r\n")
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests/student-reviews/"))
 (200
  (#"POST"
   #"https://www.captainteach.org/2166-dev/dependencies/a1-577be86f/tests/student-reviews/load")
  #"Content-Type: application/json; charset=UTF-8"
  #"")
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/dashboard/a1-577be86f/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/open/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/assignments/open"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/close/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/assignments/close"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/open/a1-577be86f/"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/delete/a1-577be86f/"))
 (#f (#"GET" #"https://www.captainteach.org/2166-dev/assignments/delete"))
 (200
  (#"GET"
   #"https://www.captainteach.org/2166-dev/assignments/dashboard/a1-577be86f/")))
