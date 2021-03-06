Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run Weather "KLAX" ["-t","<tempF>F","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 360
                    , Run Network "eth0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                    , Run Network "wlan0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%cpu% | %memory% * %swap% | %eth0% | %wlan0% | <fc=#ee9a00>%date%</fc>| %KLAX% | %uname%"
       }
