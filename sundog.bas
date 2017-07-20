10000 rem SUNDOG.BAS
10010 rem Copyright (c) 1983,2017 by Meadhbh Hamrick
10020 rem Released under a BSD 3-Clause License, see LICENSE.TXT for details
10030 rem Call the Initialize Subroutine
10040 gosub 10400
10050 rem Placeholder for a call to the test routines. See the TESTING
10060 rem section of the README.TXT file.
10070 rem Call the Location Print Subroutine
10080 gosub 10810
10090 if gg = 1 then goto 10190
10100 if gg = 2 then goto 10170
10110 rem Call the Get Command Subroutine
10120 gosub 11230
10130 rem Call the Parse Command Subrotine
10140 gosub 11340
10150 rem Jump back to the beginning and do it again
10160 goto 10080
10170 print "YOU HAVE DIED. SCORE = ";s
10180 goto 10200
10190 print "YOU WON! SCORE = ";s
10200 bye
10210 rem SUBROUTINE: Initialize the Game
10220 rem L        - Current Location
10230 rem C$       - Command String
10240 rem S        - Score
10250 rem DC, D$() - Direction Count, Direction Array
10260 rem CC, C$() - Command Count, Command Array
10270 rem OC, O$() - Object Count, Object Array
10280 rem OL(),OF()- Object Location Array, Object Flags Array
10290 rem LC, L$() - Location Count, Location Array
10300 rem LD$(),A()- Location Description Array, Adjacency Array
10310 rem LN()     - Which LD$() elements are for each room?
10320 rem LV()     - Location Visited Array
10330 rem BPV      - Bits Per Visited Array Element
10340 rem VC       - Size of the Visited Array
10350 rem NC,N$()  - Size of typed command array, typed command array
10360 rem SC,SE()  - Number of Score Elements, Score Elements
10370 rem V        - Are you wearing the Vacc Suit? 0-no, 1-yes
10380 rem A        - Did you inject the Antidote? 0-no, 1-yes
10390 rem T        - Is the teather attached? 0-no, 1-yes
10400 bpv = 16 : nc = 5 : dim n$(nc) : sc = 9 : dim se(sc)
10410 rem Read Directions
10420 restore 13830
10430 read dc : dim d$(dc)
10440 for i = 0 to dc-1 : read d$(i) : next i
10450 rem Read Commands
10460 read cc : dim c$(cc)
10470 for i = 0 to cc-1 : read c$(i) : next i
10480 rem Read Objects
10490 read oc : dim o$(oc),ol(oc),of(oc)
10500 for i = 0 to oc-1 : read o$(i),ol(i),of(i) : next i
10510 rem Read Locations
10520 read lc,i : dim l$(lc),ld$(i),a(dc*lc),ln(lc+1)
10530 for i = 0 to lc-1
10540   read l$(i),ln(i),k
10550   for j = 0 to k-1
10560     read ld$(j+ln(i))
10570   next j
10580   for j = 0 to dc-1
10590     read a(i*dc+j)
10600   next j
10610 next i
10620 ln(lc) = ln(lc-1)+k
10630 rem Initialize Visited Array
10640 vc = int(lc/bpv)
10650 if vc*bpv = lc then 10650
10660 vc = vc+1
10670 dim v(vc)
10680 gosub 10700
10690 return
10700 rem SUBROUTINE: Reset Score and Location, Print intro message
10710 l = 11 : s = 0 : v = 0 : a = 0 : t = 0 : gg = 0 : turn = 0
10720 for i = 0 to vc-1 : v(i) = 0 : next i
10730 return
10740 rem SUBROUTINE: YOU HAVE DIED
10750 print "YOU HAVE DIED"
10760 gosub 11770
10770 gosub 10700
10780 gg = 2
10790 return
10800 rem SUBROUTINE: Print Location If You've Never Been Here
10810 gosub 11280
10820 print : print l$(l)
10830 if k <> 0 then 10850
10840 gosub 10910
10850 gosub 10960
10860 gosub 11100
10870 if l <> 19 then goto 10890
10880 gg = 1
10890 return
10900 rem SUBROUTINE: Print Location
10910 for i = ln(l) to ln(l+1)-1
10920   print ld$(i)
10930 next i
10940 return
10950 rem SUBROUTINE: Print Items in this Location
10960 i = 0
10970 for j = 0 to oc-1
10980   if (ol(j) = l) and ((of(j) and 1) = 1) then i = i+1
10990 next j
11000 if i = 0 then goto 11080
11010 print "OBJECTS HERE: ";
11020 for j = 0 to oc-1
11030   if ol(j) <> l then goto 11060
11040   if (of(j) and 1) <> 1 then goto 11060
11050   print o$(j);" ";
11060 next j
11070 print
11080 return
11090 rem SUBROUTINE: Print Warnings
11100 if (turn < 5) or (turn > 9) or (v = 1) then goto 11120
11110 print "YOU FEEL SLIGHTLY WINDED."
11120 if (turn < 10) or (v = 1) then goto 11140
11130 print "YOU FEEL REALLY TIRED."
11140 if (turn < 16) or (v = 1) then goto 11170
11150 print "YOU HAVE DIED OF ASPHYXIA." : gg = 2
11160 goto 11210
11170 if (turn < 20) or (a = 1) then goto 11190
11180 print "YOU FEEL SICK TO YOUR STOMACH."
11190 if (turn < 30) or (a = 1) then goto 11210
11200 print "YOU HAVE DIED OF RADIATION POSIONING." : gg = 2
11210 return
11220 rem SUBROUTINE: Get Command Line
11230 print
11240 print "WHAT NOW ";
11250 input c$
11260 return
11270 rem SUBROUTINE: Visited? Set's K to 1 if this location's been visited.
11280 i = int(l/bpv)
11290 j = l-i*bpv
11300 if (v(i) and 2^j) = 0 then k = 0 else k = 1
11310 v(i) = v(i) or 2^j
11320 return
11330 rem SUBROUTINE: Parse & Execute Command
11340 b = 1 : nn = 0
11350 e = instr(c$," ",b)
11360 if e <> 0 then goto 11400
11370 n$(nn) = mid$(c$,b,len(c$)-b+1)
11380 nn = nn+1
11390 goto 11450
11400 n$(nn) = mid$(c$,b,e-b)
11410 b = e+1
11420 nn = nn+1
11430 if k >= nc then 11450
11440 goto 11350
11450 for i = 0 to cc-1
11460   if ucase$(n$(0)) <> c$(i) then goto 11480
11470   exit for
11480 next i
11490 if i >= cc then goto 11550
11500 g = i
11510 on g+1 gosub 11570,11630,11670,11770,11810,11840,11990,11990,12170
11520 on g-8 gosub 12290,12290,12360,12360,12430,12430,12500,12500,12570,12740
11530 on g-18 gosub 12910,13090,13390,13560,13690
11540 goto 11560
11550 print "I DIDN'T UNDERSTAND THAT COMMAND."
11560 return
11570 rem SUBROUTINE: COMMAND: 00 HELP
11580 print "MOVE AROUND THE SHIP USING COMMANDS FORE, STARBOARD, AFT"
11590 print "AND PORT. GRAB ITEMS WITH THE COMMANDS GET OR TAKE. SAY"
11600 print '"QUIT" WHEN YOU'RE DONE. USE THE COMMAND "INVENTORY" TO GET'
11610 print "A LIST OF ITEMS YOU'RE CARRYING."
11620 return
11630 rem SUBROUTINE: COMMAND: 01 LOOK
11640 gosub 10910
11650 gosub 10960
11660 return
11670 rem SUBROUTINE: COMMAND: 02 INVENTORY
11680 print "YOU ARE CURRENTLY CARRYING:"
11690 j = 0
11700 for i = 0 to oc
11710   if ol(i) <> -1 then 11740
11720   print "  ";o$(i)
11730   j = j+1
11740 next i
11750 if j = 0 then print "  *YOU'RE NOT CARRYING ANYTHING*"
11760 return
11770 rem SUBROUTINE: COMMAND: 03 SCORE
11780 s = 0 : for i = 0 to sc-1 : s = s+se(i) : next i
11790 print "SCORE: ";s
11800 return
11810 rem SUBROUTINE: COMMAND: 04 QUIT
11820 bye
11830 return
11840 rem SUBROUTINE: COMMAND: 05 GO
11850 if nn > 1 then 11880
11860 print "GO WHERE?"
11870 goto 11980
11880 for i = 0 to dc-1
11890   if ucase$(n$(1)) <> d$(i) then goto 11910
11900   exit for
11910 next i
11920 if i >= dc then goto 11860
11930 if a(l*dc+i) >= 0 then goto 11960
11940 print "YOU CAN'T GO THAT WAY"
11950 goto 11980
11960 l = a(l*dc+i)
11970 turn = turn+1
11980 return
11990 rem SUBROUTINE: COMMAND: 06,07 GET, TAKE
12000 for i = 0 to oc-1
12010   if ucase$(n$(1)) <> o$(i) then 12030
12020   exit for
12030 next i
12040 if i < oc then 12070
12050 print "I SEE NO ";ucase$(n$(1));" HERE"
12060 goto 12160
12070 if (of(i) and 2) = 2 then 12100
12080 print "YOU CAN'T TAKE THE ";ucase$(n$(1))
12090 goto 12160
12100 print "GOT ";o$(i)
12110 ol(i) = -1
12120 turn = turn+1
12130 if (of(i) and 128) <> 128 then 12160
12140 se(i) = 5
12150 gosub 11780
12160 return
12170 rem SUBROUTINE: COMMAND: 08 DROP
12180 for i = 0 to oc-1
12190   if ucase$(n$(1)) <> o$(i) then 12210
12200   exit for
12210 next i
12220 if i < oc then 12250
12230 print "I DON'T THINK YOU'RE CARRYING THAT."
12240 goto 12160
12250 print "DROPPED ";o$(i)
12260 ol(i) = l
12270 turn = turn+1
12280 return
12290 rem SUBROUTINE: COMMAND: 09,10 FORE,F
12300 if a(l*dc) >= 0 then goto 12330
12310 print "YOU CAN'T GO THAT WAY"
12320 goto 12350
12330 l = a(l*dc)
12340 turn = turn+1
12350 return
12360 rem SUBROUTINE: COMMAND: 11,12 STARBOARD,S
12370 if a(l*dc+1) >= 0 then goto 12400
12380 print "YOU CAN'T GO THAT WAY"
12390 goto 12420
12400 l = a(l*dc+1)
12410 turn = turn+1
12420 return
12430 rem SUBROUTINE: COMMAND: 13,14 AFT,A
12440 if a(l*dc+2) >= 0 then goto 12470
12450 print "YOU CAN'T GO THAT WAY"
12460 goto 12490
12470 l = a(l*dc+2)
12480 turn = turn+1
12490 return
12500 rem SUBROUTINE: COMMAND: 15,16 PORT,P
12510 if a(l*dc+3) >= 0 then goto 12540
12520 print "YOU CAN'T GO THAT WAY"
12530 goto 12560
12540 l = a(l*dc+3)
12550 turn = turn+1
12560 return
12570 rem SUBROUTINE: COMMAND: 17 UNLOCK
12580 if nn > 1 then 12610
12590 print "WHAT DO YOU WANT ME TO UNLOCK?"
12600 goto 12730
12610 if ucase$(n$(1)) <> "VALVE" then goto 12720
12620 if l = 3 then goto 12650
12630 print "I SEE NO VALVE HERE"
12640 goto 12730
12650 if ol(0) = -1 then 12680
12660 print "YOU DON'T HAVE ANYTHING TO UNLOCK THE VALVE WITH"
12670 goto 12730
12680 print "VALVE UNLOCKED"
12690 a(3*dc) = 1 : a(dc+2) = 3
12700 turn = turn+1
12710 goto 12730
12720 print "I DON'T KNOW HOW TO UNLOCK THAT"
12730 return
12740 rem SUBROUTINE: COMMAND: 18 OPEN
12750 if nn > 1 then 12780
12760 print "WHAT DO YOU WANT ME TO OPEN?"
12770 goto 12900
12780 if ucase$(n$(1)) <> "HATCH" then goto 12890
12790 if l = 1 then goto 12820
12800 print "I SEE NO HATCH HERE"
12810 goto 12900
12820 if ol(4) = -1 then 12850
12830 print "YOU DON'T HAVE ANYTHING TO OPEN THE HATCH WITH"
12840 goto 12900
12850 print "HATCH OPENED"
12860 a(dc) = 0 : a(2) = 1
12870 turn = turn+1
12880 goto 12900
12890 print "I DON'T KNOW HOW TO OPEN THAT"
12900 return
12910 rem SUBROUTINE: COMMAND: 19 WEAR
12920 if nn > 1 then goto 12950
12930 print "WHAT DO YOU WANT TO WEAR?"
12940 goto 13080
12950 if ucase$(n$(1)) = "VACCSUIT" then goto 12980
12960 print "YOU CAN'T WEAR THE ";ucase$(n$(1))
12970 goto 13080
12980 if v = 0 then goto 13010
12990 print "YOU'RE ALREADY WEARING THE VACCSUIT"
13000 goto 13080
13010 if ol(1) = -1 then goto 13040
13020 print "YOU DON'T HAVE THE VACCSUIT"
13030 goto 13080
13040 print "YOU PUT THE VACCSUIT ON"
13050 v = 1 : turn = turn+1
13060 se(6) = 10
13070 gosub 11770
13080 return
13090 rem SUBROUTINE: COMMAND: 20 ATTACH
13100 if nn > 1 then goto 13130
13110 print "WHAT DO YOU WANT TO ATTACH?"
13120 goto 13380
13130 if ucase$(n$(1)) = "TEATHER" then goto 13160
13140 print "HOW WOULD YOU ATTACH THE ";ucase$(n$(1));"?"
13150 goto 13380
13160 if t = 0 then goto 13190
13170 print "YOU'RE ALREADY ATTACHED"
13180 goto 13380
13190 if ol(2) = -1 then goto 13220
13200 print "YOU DON'T HAVE THE TEATHER"
13210 goto 13380
13220 if (nn > 3) and (ucase$(n$(2)) = "TO") and (ucase$(n$(3)) = "VACCSUIT") then goto 13250
13230 print "DO YOU WANT TO ATTACH THE TEATHER TO ANYTHING?"
13240 goto 13380
13250 if ol(1) = -1 then goto 13280
13260 print "YOU DON'T HAVE THE VACCSUIT"
13270 goto 13380
13280 if l = 4 then goto 13310
13290 print "YOU DON'T SEE ANYTHING TO ATTACH TO"
13300 goto 13380
13310 if t = 0 then goto 13340
13320 print "YOU'RE ALREADY ATTACHED"
13330 goto 13380
13340 print "YOU ATTACH THE VACCSUIT TO A HANDY RING IN THE AIRLOCK"
13350 t = 1 : turn = turn+1
13360 se(7) = 10
13370 gosub 11770
13380 return
13390 rem SUBROUTINE: COMMAND: 21 EXIT
13400 if nn > 1 then goto 13430
13410 print "EXIT WHAT?"
13420 goto 13550
13430 if (l = 4) and (ucase$(n$(1)) = "AIRLOCK") then goto 13460
13440 print "WHAT DO YOU WANT TO EXIT?"
13450 goto 13550
13460 print "YOU HAVE EXITED THE SHIP"
13470 if t = 1 then goto 13510
13480 print "UNFORTUNATELY, YOU WEREN'T ATTACHED TO ANYTHING"
13490 gosub 10740
13500 goto 13550
13510 l = 5
13520 se(8) = 20
13530 turn = turn+1
13540 gosub 11770
13550 return
13560 rem SUBROUTINE: COMMAND: 22 ENTER
13570 if nn > 1 then goto 13600
13580 print "ENTER WHAT?"
13590 goto 13680
13600 if (l = 5) and (ucase$(n$(1)) = "DERELICT") then goto 13630
13610 print "WHAT DO YOU WANT TO ENTER?"
13620 goto 13680
13630 print "YOU HAVE ENTERED THE DERELICT"
13640 l = 19
13650 se(9) = 20
13660 turn = turn+1
13670 gosub 11770
13680 return
13690 rem SUBROUTINE: COMMAND: 23 DRINK
13700 if nn > 1 then goto 13730
13710 print "DRINK WHAT?"
13720 goto 13810
13730 if ucase$(n$(1)) = "ANTIDOTE" then goto 13760
13740 print "I'M NOT SURE I CAN DRINK THAT."
13750 goto 13810
13760 if ol(3) = -1 then goto 13790
13770 print "YOU DON'T HAVE THE ANTIDOTE"
13780 goto 13810
13790 a = 1
13800 print "YOU DRANK THE ANTIDOTE. YOU FEEL MUCH BETTER."
13810 return
13820 rem Directions (note. this changes between SUNDOG1, SUNDOG2 & SUNDOG3
13830 data 4,"FORE","STARBOARD","AFT","PORT"
13840 rem Commands
13850 data 24
13860 data "HELP","LOOK","INVENTORY","SCORE","QUIT","GO","GET","TAKE","DROP"
13870 data "FORE","F","STARBOARD","S","AFT","A","PORT","P","UNLOCK","OPEN"
13880 data "WEAR","ATTACH","EXIT","ENTER","DRINK"
13890 rem Objects
13900 data 8
13910 data "KEYCARD",8,131
13920 data "VACCSUIT",2,143
13930 data "TEATHER",14,131
13940 data "ANTIDOTE",6,131
13950 data "MULTITOOL",17,131
13960 data "ANSIBLE",0,131
13970 data "VALVE",3,96
13980 data "HATCH",1,4
13990 rem Locations
14000 data 20,54
14010 data "Avionics Bay",0,2
14020 data "This  is a  cramped enclosure  stuffed with  electronics gear.  A"
14030 data "crawlway leads aft."
14040 data -1,-1,-1,-1
14050 data "Bridge",2,4
14060 data "This  is  the control  center  of  the ship.  Surprisingly  small"
14070 data "windows look out into the inky  blackness of space. A small hatch"
14080 data "is on the floor to the fore while a large pressure door is on the"
14090 data "bulkhead aft."
14100 data -1,-1,-1,-1
14110 data "Ship's Locker",6,2
14120 data 'The "Junk Drawer"  of the ship, all sorts of  odds-n-ends wind up'
14130 data "here. The only exit is to starboard."
14140 data -1,3,-1,-1
14150 data "Fore Passageway",8,3
14160 data "This is a  non-descript passageway with a large  iris valve fore,"
14170 data "an airlock  door to  the starboard  and a small  door to  port. A"
14180 data "similar passageway lies aft."
14190 data -1,4,7,2
14200 data "Airlock",11,2
14210 data "This  is a  smallish  airlock, with  several  blinking buttons  &"
14220 data "attachment points."
14230 data -1,-1,-1,3
14240 data "Space",13,2
14250 data "You are floating in the inky blackness of space. A derelict alien"
14260 data "craft floats beside you."
14270 data -1,-1,-1,-1
14280 data "Sickbay",15,2
14290 data "Really little more than a glorified medicine cabinet, the sickbay"
14300 data "may contain useful supplies."
14310 data -1,7,9,-1
14320 data "Mid Passageway",17,2
14330 data "This is  a non-descript  passage with  doors to  port and  aft. A"
14340 data "similar passageway lies fore."
14350 data 3,-1,10,6
14360 data "Captain's Stateroom",19,3
14370 data "Just  large enough  to  fit  a bunk  and  a  desk, the  captain's"
14380 data "stateroom is  what passes for  luxury around here. A  single door"
14390 data "leads aft. A hissing sound may be heard behind the bunk."
14400 data -1,-1,11,-1
14410 data "Laboratory",22,2
14420 data "The  ship's lab  is intended  mostly  to identify  geo- and  bio-"
14430 data "samples."
14440 data 6,-1,-1,-1
14450 data "Ward Room",24,4
14460 data "This smallish room  is where you spend most of  your down-time on"
14470 data "the ship.  A non-functional  television screen  is bolted  to the"
14480 data "wall. A passageways fore and aft exit the room. To the starboard,"
14490 data "the galley can be seen."
14500 data 7,11,14,-1
14510 data "Galley",28,3
14520 data "This  is  where  the  crew   cook  their  meals  and  wash  their"
14530 data 'dishes.  And by  "crew," I  mean  you. To  the port  is the  ward'
14540 data "room. Doors fore, starboard and aft lead to crew quarters."
14550 data 8,12,13,10
14560 data "Navigator's Bunk",31,2
14570 data "This is more  like an inflatable mattress stuffed  into a kitchen"
14580 data "pantry. A single door to the port leads to the galley."
14590 data -1,-1,-1,11
14600 data "Engineer's Quarters",33,2
14610 data "The  engineer's quarters  are  just large  enough  for a  private"
14620 data "bunk. A door fore leads to the galley."
14630 data 11,-1,-1,-1
14640 data "Cargo Hold",35,4
14650 data 'The largest "room" on the ship, the cargo hold contained supplies'
14660 data "of all sorts. But supplies are running low. Hatches lead fore and"
14670 data "aft.  The entire  starboard  wall  is an  access  hatch into  the"
14680 data "Air/Raft Berth."
14690 data 10,-1,17,-1
14700 data "Air-Raft Berth",39,3
14710 data "This area  holds the  Air/Raft normally. The  raft appears  to be"
14720 data "missing. Safety interlocks  prevent the hatch into  the cargo bay"
14730 data "from opening unless the air raft returns."
14740 data -1,-1,-1,-1
14750 data "Thrusters",42,3
14760 data "You wouldn't think it was  possible, but you can actually squeeze"
14770 data "into  the thruster  machinery. The  only possible  exit is  to go"
14780 data "starboard back to the engine room."
14790 data -1,17,-1,-1
14800 data "Engine Room",45,3
14810 data "From the engine room, the crew has access to the drive mechanisms"
14820 data "that power  the ship. You can  squeeze into the thrusters  to the"
14830 data "port or the jump drive to the starboard."
14840 data 14,18,-1,16
14850 data "Jump Drive",48,4
14860 data "The jump drive powers the  ship's interstellar voyages. The drive"
14870 data "mechanism itself is safely sealed  within a durable shell. A sign"
14880 data "on the  drive explains  that there are  no user  servicable parts"
14890 data "inside."
14900 data -1,-1,-1,17
14910 data "Derelict Alien Craft",52,3
14920 data "You have  found your way  into the Derelict Alien  Craft. Bizarre"
14930 data "forms adorn  every surface,  though artificial  gravity, thruster"
14940 data "and interstellar drive mechanisms appear obvious."
14950 data -1,-1,-1,-1
