*  PSID DATA CENTER *****************************************************
   JOBID            : 294151                            
   DATA_DOMAIN      : TA                                
   USER_WHERE       : NULL                              
   FILE_TYPE        : NULL                              
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : sps                               
   CODEBOOK_TYPE    : NULL                              
   N_OF_VARIABLES   : 819                               
   N_OF_OBSERVATIONS: 4058                              
   MAX_REC_LENGTH   : 1003                              
   DATE & TIME      : June 9, 2021 @ 10:05:47
*************************************************************************
.

FILE HANDLE myfile /
   NAME = "[path]\J294151.txt" LRECL = 1003 .

DATA LIST FILE = myfile FIXED /
      TAS                  1 - 1           TAS05                2 - 2           TAS07                3 - 3     
      TAS09                4 - 4           TAS11                5 - 5           TAS13                6 - 6     
      TAS15                7 - 7           TAS17                8 - 8           ER30001              9 - 12    
      ER30002             13 - 15          ER33801             16 - 20          ER33802             21 - 22    
      ER33803             23 - 24          TA050001            25 - 25          TA050015            26 - 26    
      TA050016            27 - 30          TA050017            31 - 31          TA050018            32 - 32    
      TA050019            33 - 33          TA050020            34 - 34          TA050021            35 - 35    
      TA050022            36 - 36          TA050023            37 - 37          TA050024            38 - 38    
      TA050025            39 - 39          TA050026            40 - 40          TA050027            41 - 41    
      TA050028            42 - 42          TA050029            43 - 43          TA050030            44 - 44    
      TA050031            45 - 45          TA050032            46 - 46          TA050033            47 - 47    
      TA050034            48 - 48          TA050035            49 - 49          TA050036            50 - 50    
      TA050037            51 - 51          TA050038            52 - 52          TA050039            53 - 53    
      TA050040            54 - 54          TA050041            55 - 55          TA050042            56 - 56    
      TA050043            57 - 58          TA050044            59 - 59          TA050045            60 - 60    
      TA050046            61 - 61          TA050047            62 - 62          TA050048            63 - 63    
      TA050049            64 - 64          TA050050            65 - 65          TA050051            66 - 66    
      TA050052            67 - 67          TA050053            68 - 68          TA050054            69 - 69    
      TA050055            70 - 70          TA050056            71 - 71          TA050057            72 - 72    
      TA050058            73 - 73          TA050059            74 - 74          TA050060            75 - 75    
      TA050061            76 - 76          TA050069            77 - 77          TA050078            78 - 78    
      TA050091            79 - 80          TA050127            81 - 82          TA050131            83 - 83    
      TA050368            84 - 84          TA050394            85 - 86          TA050573            87 - 87    
      TA050594            88 - 88          TA050595            89 - 89          TA050631            90 - 90    
      TA050678            91 - 91          TA050708            92 - 92          TA050710            93 - 93    
      TA050711            94 - 94          TA050712            95 - 95          TA050713            96 - 96    
      TA050714            97 - 97          TA050715            98 - 98          TA050716            99 - 99    
      TA050717           100 - 100         TA050723           101 - 101         TA050733           102 - 102   
      TA050734           103 - 103         TA050754           104 - 105         TA050755           106 - 106   
      TA050756           107 - 107         TA050759           108 - 110         TA050767           111 - 111   
      TA050768           112 - 113         TA050769           114 - 116         TA050776           117 - 117   
      TA050777           118 - 118         TA050784           119 - 119         TA050785           120 - 120   
      TA050792           121 - 121         TA050793           122 - 122         TA050797           123 - 123   
      TA050798           124 - 124         TA050808           125 - 125         TA050809           126 - 126   
      TA050816           127 - 127         TA050817           128 - 128         TA050824           129 - 129   
      TA050825           130 - 130         TA050883           131 - 131         TA050884           132 - 132   
      TA050888           133 - 133         TA050889           134 - 134         TA050890           135 - 135   
      TA050891           136 - 136         TA050892           137 - 137         TA050893           138 - 138   
      TA050894           139 - 139         TA050895           140 - 140         TA050896           141 - 141   
      TA050897           142 - 142         TA050898           143 - 143         TA050899           144 - 144   
      TA050900           145 - 145         TA050901           146 - 146         TA050932           147 - 147   
      TA050933           148 - 148         TA050935           149 - 149         TA050937           150 - 150   
      TA050938           151 - 152         TA050944           153 - 156         TA050946           157 - 158   
      ER33901            159 - 163         ER33902            164 - 165         ER33903            166 - 167   
      TA070001           168 - 168         TA070015           169 - 169         TA070016           170 - 173   
      TA070017           174 - 174         TA070018           175 - 175         TA070019           176 - 176   
      TA070020           177 - 177         TA070021           178 - 178         TA070022           179 - 179   
      TA070023           180 - 180         TA070024           181 - 181         TA070025           182 - 182   
      TA070026           183 - 183         TA070027           184 - 184         TA070028           185 - 185   
      TA070029           186 - 186         TA070030           187 - 187         TA070031           188 - 188   
      TA070032           189 - 189         TA070033           190 - 190         TA070034           191 - 191   
      TA070035           192 - 192         TA070036           193 - 193         TA070037           194 - 194   
      TA070038           195 - 195         TA070039           196 - 196         TA070040           197 - 197   
      TA070041           198 - 198         TA070042           199 - 199         TA070043           200 - 201   
      TA070044           202 - 202         TA070045           203 - 203         TA070046           204 - 204   
      TA070047           205 - 205         TA070048           206 - 206         TA070049           207 - 207   
      TA070050           208 - 208         TA070051           209 - 209         TA070052           210 - 210   
      TA070053           211 - 211         TA070054           212 - 212         TA070055           213 - 213   
      TA070056           214 - 214         TA070057           215 - 215         TA070058           216 - 216   
      TA070059           217 - 217         TA070060           218 - 218         TA070061           219 - 219   
      TA070069           220 - 220         TA070078           221 - 221         TA070091           222 - 223   
      TA070127           224 - 225         TA070131           226 - 226         TA070341           227 - 227   
      TA070368           228 - 229         TA070548           230 - 230         TA070569           231 - 231   
      TA070570           232 - 232         TA070602           233 - 233         TA070649           234 - 234   
      TA070679           235 - 235         TA070681           236 - 236         TA070682           237 - 237   
      TA070683           238 - 238         TA070684           239 - 239         TA070685           240 - 240   
      TA070686           241 - 241         TA070687           242 - 242         TA070688           243 - 243   
      TA070694           244 - 244         TA070704           245 - 245         TA070705           246 - 246   
      TA070725           247 - 248         TA070726           249 - 249         TA070727           250 - 250   
      TA070730           251 - 253         TA070738           254 - 254         TA070739           255 - 256   
      TA070740           257 - 259         TA070747           260 - 260         TA070748           261 - 261   
      TA070755           262 - 262         TA070756           263 - 263         TA070763           264 - 264   
      TA070764           265 - 265         TA070768           266 - 266         TA070769           267 - 267   
      TA070776           268 - 268         TA070777           269 - 269         TA070784           270 - 270   
      TA070785           271 - 271         TA070792           272 - 272         TA070793           273 - 273   
      TA070864           274 - 274         TA070865           275 - 275         TA070869           276 - 276   
      TA070870           277 - 277         TA070871           278 - 278         TA070872           279 - 279   
      TA070873           280 - 280         TA070874           281 - 281         TA070875           282 - 282   
      TA070876           283 - 283         TA070877           284 - 284         TA070878           285 - 285   
      TA070879           286 - 286         TA070880           287 - 287         TA070881           288 - 288   
      TA070882           289 - 289         TA070913           290 - 290         TA070914           291 - 291   
      TA070916           292 - 292         TA070918           293 - 293         TA070919           294 - 295   
      TA070925           296 - 299         TA070927           300 - 301         ER34001            302 - 306   
      ER34002            307 - 308         ER34003            309 - 310         TA090001           311 - 311   
      TA090016           312 - 312         TA090017           313 - 316         TA090018           317 - 317   
      TA090019           318 - 318         TA090020           319 - 319         TA090021           320 - 320   
      TA090022           321 - 321         TA090023           322 - 322         TA090024           323 - 323   
      TA090025           324 - 324         TA090026           325 - 325         TA090027           326 - 326   
      TA090028           327 - 327         TA090029           328 - 328         TA090030           329 - 329   
      TA090031           330 - 330         TA090032           331 - 331         TA090033           332 - 332   
      TA090034           333 - 333         TA090035           334 - 334         TA090036           335 - 335   
      TA090037           336 - 336         TA090038           337 - 337         TA090039           338 - 338   
      TA090040           339 - 339         TA090041           340 - 340         TA090042           341 - 341   
      TA090043           342 - 342         TA090044           343 - 344         TA090045           345 - 345   
      TA090046           346 - 346         TA090047           347 - 347         TA090048           348 - 348   
      TA090049           349 - 349         TA090050           350 - 350         TA090051           351 - 351   
      TA090052           352 - 352         TA090053           353 - 353         TA090054           354 - 354   
      TA090055           355 - 355         TA090056           356 - 356         TA090057           357 - 357   
      TA090058           358 - 358         TA090059           359 - 359         TA090060           360 - 360   
      TA090061           361 - 361         TA090062           362 - 362         TA090078           363 - 363   
      TA090087           364 - 364         TA090100           365 - 366         TA090136           367 - 368   
      TA090140           369 - 369         TA090358           370 - 370         TA090385           371 - 372   
      TA090590           373 - 373         TA090611           374 - 374         TA090612           375 - 375   
      TA090655           376 - 376         TA090705           377 - 377         TA090735           378 - 378   
      TA090737           379 - 379         TA090738           380 - 380         TA090739           381 - 381   
      TA090740           382 - 382         TA090741           383 - 383         TA090742           384 - 384   
      TA090743           385 - 385         TA090744           386 - 386         TA090750           387 - 387   
      TA090760           388 - 388         TA090761           389 - 389         TA090784           390 - 391   
      TA090785           392 - 392         TA090786           393 - 393         TA090789           394 - 396   
      TA090797           397 - 397         TA090798           398 - 399         TA090799           400 - 402   
      TA090806           403 - 403         TA090807           404 - 404         TA090814           405 - 405   
      TA090815           406 - 406         TA090822           407 - 407         TA090823           408 - 408   
      TA090827           409 - 409         TA090828           410 - 410         TA090835           411 - 411   
      TA090836           412 - 412         TA090843           413 - 413         TA090844           414 - 414   
      TA090851           415 - 415         TA090852           416 - 416         TA090924           417 - 417   
      TA090925           418 - 418         TA090929           419 - 419         TA090930           420 - 420   
      TA090931           421 - 421         TA090932           422 - 422         TA090933           423 - 423   
      TA090934           424 - 424         TA090935           425 - 425         TA090936           426 - 426   
      TA090937           427 - 427         TA090938           428 - 428         TA090939           429 - 429   
      TA090940           430 - 430         TA090941           431 - 431         TA090942           432 - 432   
      TA090977           433 - 433         TA090978           434 - 434         TA090980           435 - 435   
      TA090982           436 - 436         TA090983           437 - 438         TA090989           439 - 442   
      TA090991           443 - 444         ER34101            445 - 449         ER34102            450 - 451   
      ER34103            452 - 453         TA110001           454 - 454         TA110016           455 - 455   
      TA110017           456 - 459         TA110018           460 - 460         TA110019           461 - 461   
      TA110020           462 - 462         TA110021           463 - 463         TA110022           464 - 464   
      TA110023           465 - 465         TA110024           466 - 466         TA110025           467 - 467   
      TA110026           468 - 468         TA110027           469 - 469         TA110028           470 - 470   
      TA110029           471 - 471         TA110030           472 - 472         TA110031           473 - 473   
      TA110032           474 - 474         TA110033           475 - 475         TA110034           476 - 476   
      TA110035           477 - 477         TA110036           478 - 478         TA110037           479 - 479   
      TA110038           480 - 480         TA110039           481 - 481         TA110040           482 - 482   
      TA110041           483 - 483         TA110042           484 - 484         TA110043           485 - 485   
      TA110044           486 - 487         TA110045           488 - 489         TA110046           490 - 490   
      TA110047           491 - 491         TA110048           492 - 492         TA110049           493 - 493   
      TA110050           494 - 494         TA110051           495 - 495         TA110052           496 - 496   
      TA110053           497 - 497         TA110054           498 - 498         TA110055           499 - 499   
      TA110056           500 - 500         TA110057           501 - 501         TA110058           502 - 502   
      TA110059           503 - 503         TA110060           504 - 504         TA110061           505 - 505   
      TA110062           506 - 506         TA110063           507 - 507         TA110079           508 - 508   
      TA110088           509 - 509         TA110101           510 - 511         TA110137           512 - 513   
      TA110141           514 - 514         TA110348           515 - 515         TA110462           516 - 517   
      TA110671           518 - 518         TA110698           519 - 519         TA110699           520 - 520   
      TA110743           521 - 521         TA110793           522 - 522         TA110825           523 - 523   
      TA110827           524 - 524         TA110828           525 - 525         TA110829           526 - 526   
      TA110830           527 - 527         TA110831           528 - 528         TA110832           529 - 529   
      TA110833           530 - 530         TA110834           531 - 531         TA110842           532 - 532   
      TA110853           533 - 533         TA110854           534 - 534         TA110900           535 - 536   
      TA110901           537 - 537         TA110902           538 - 538         TA110905           539 - 541   
      TA110913           542 - 542         TA110914           543 - 544         TA110915           545 - 547   
      TA110922           548 - 548         TA110923           549 - 549         TA110930           550 - 550   
      TA110931           551 - 551         TA110938           552 - 552         TA110939           553 - 553   
      TA110943           554 - 554         TA110944           555 - 555         TA110951           556 - 556   
      TA110952           557 - 557         TA110959           558 - 558         TA110960           559 - 559   
      TA110967           560 - 560         TA110968           561 - 561         TA111056           562 - 562   
      TA111057           563 - 563         TA111061           564 - 564         TA111062           565 - 565   
      TA111063           566 - 566         TA111064           567 - 567         TA111065           568 - 568   
      TA111066           569 - 569         TA111067           570 - 570         TA111068           571 - 571   
      TA111069           572 - 572         TA111070           573 - 573         TA111071           574 - 574   
      TA111072           575 - 575         TA111073           576 - 576         TA111074           577 - 577   
      TA111119           578 - 578         TA111120           579 - 579         TA111122           580 - 580   
      TA111124           581 - 581         TA111125           582 - 583         TA111131           584 - 587   
      TA111133           588 - 589         ER34201            590 - 594         ER34202            595 - 596   
      ER34203            597 - 598         TA130001           599 - 599         TA130016           600 - 600   
      TA130017           601 - 604         TA130018           605 - 605         TA130019           606 - 606   
      TA130020           607 - 607         TA130021           608 - 608         TA130022           609 - 609   
      TA130023           610 - 610         TA130024           611 - 611         TA130025           612 - 612   
      TA130026           613 - 613         TA130027           614 - 614         TA130028           615 - 615   
      TA130029           616 - 616         TA130030           617 - 617         TA130031           618 - 618   
      TA130032           619 - 619         TA130033           620 - 620         TA130034           621 - 621   
      TA130035           622 - 622         TA130036           623 - 623         TA130037           624 - 624   
      TA130038           625 - 625         TA130039           626 - 626         TA130040           627 - 627   
      TA130041           628 - 628         TA130042           629 - 629         TA130043           630 - 631   
      TA130044           632 - 633         TA130045           634 - 634         TA130046           635 - 635   
      TA130047           636 - 636         TA130048           637 - 637         TA130049           638 - 638   
      TA130050           639 - 639         TA130051           640 - 640         TA130052           641 - 641   
      TA130053           642 - 642         TA130054           643 - 643         TA130055           644 - 644   
      TA130056           645 - 645         TA130057           646 - 646         TA130058           647 - 647   
      TA130059           648 - 648         TA130060           649 - 649         TA130061           650 - 650   
      TA130062           651 - 651         TA130078           652 - 652         TA130087           653 - 653   
      TA130100           654 - 655         TA130136           656 - 657         TA130140           658 - 658   
      TA130347           659 - 659         TA130482           660 - 661         TA130691           662 - 662   
      TA130718           663 - 663         TA130719           664 - 664         TA130763           665 - 665   
      TA130813           666 - 666         TA130848           667 - 667         TA130850           668 - 668   
      TA130851           669 - 669         TA130852           670 - 670         TA130853           671 - 671   
      TA130854           672 - 672         TA130855           673 - 673         TA130856           674 - 674   
      TA130857           675 - 675         TA130866           676 - 676         TA130877           677 - 677   
      TA130878           678 - 678         TA130933           679 - 680         TA130934           681 - 681   
      TA130935           682 - 682         TA130938           683 - 685         TA130946           686 - 686   
      TA130947           687 - 688         TA130948           689 - 691         TA130955           692 - 692   
      TA130956           693 - 693         TA130963           694 - 694         TA130964           695 - 695   
      TA130971           696 - 696         TA130972           697 - 697         TA130976           698 - 698   
      TA130977           699 - 699         TA130984           700 - 700         TA130985           701 - 701   
      TA130992           702 - 702         TA130993           703 - 703         TA131000           704 - 704   
      TA131001           705 - 705         TA131091           706 - 706         TA131092           707 - 707   
      TA131097           708 - 708         TA131098           709 - 709         TA131099           710 - 710   
      TA131100           711 - 711         TA131101           712 - 712         TA131102           713 - 713   
      TA131103           714 - 714         TA131104           715 - 715         TA131105           716 - 716   
      TA131106           717 - 717         TA131107           718 - 718         TA131108           719 - 719   
      TA131109           720 - 720         TA131110           721 - 721         TA131211           722 - 722   
      TA131212           723 - 723         TA131214           724 - 724         TA131216           725 - 725   
      TA131217           726 - 727         TA131223           728 - 731         TA131225           732 - 733   
      ER34301            734 - 738         ER34302            739 - 740         ER34303            741 - 742   
      TA150001           743 - 743         TA150016           744 - 744         TA150017           745 - 748   
      TA150018           749 - 749         TA150019           750 - 750         TA150020           751 - 751   
      TA150021           752 - 752         TA150022           753 - 753         TA150023           754 - 754   
      TA150024           755 - 755         TA150025           756 - 756         TA150026           757 - 757   
      TA150027           758 - 758         TA150028           759 - 759         TA150029           760 - 760   
      TA150030           761 - 761         TA150031           762 - 762         TA150032           763 - 763   
      TA150033           764 - 764         TA150034           765 - 765         TA150035           766 - 766   
      TA150036           767 - 767         TA150037           768 - 768         TA150038           769 - 769   
      TA150039           770 - 770         TA150040           771 - 771         TA150041           772 - 772   
      TA150042           773 - 773         TA150043           774 - 775         TA150044           776 - 777   
      TA150045           778 - 778         TA150046           779 - 779         TA150047           780 - 780   
      TA150048           781 - 781         TA150049           782 - 782         TA150050           783 - 783   
      TA150051           784 - 784         TA150052           785 - 785         TA150053           786 - 786   
      TA150054           787 - 787         TA150055           788 - 788         TA150056           789 - 789   
      TA150057           790 - 790         TA150058           791 - 791         TA150059           792 - 792   
      TA150060           793 - 793         TA150061           794 - 794         TA150062           795 - 795   
      TA150070           796 - 796         TA150079           797 - 797         TA150092           798 - 799   
      TA150128           800 - 801         TA150132           802 - 802         TA150349           803 - 803   
      TA150491           804 - 805         TA150701           806 - 806         TA150730           807 - 807   
      TA150731           808 - 808         TA150776           809 - 809         TA150826           810 - 810   
      TA150865           811 - 811         TA150867           812 - 812         TA150868           813 - 813   
      TA150869           814 - 814         TA150870           815 - 815         TA150871           816 - 816   
      TA150872           817 - 817         TA150873           818 - 818         TA150874           819 - 819   
      TA150883           820 - 820         TA150892           821 - 821         TA150893           822 - 822   
      TA150949           823 - 824         TA150950           825 - 825         TA150951           826 - 826   
      TA150954           827 - 829         TA150968           830 - 830         TA150969           831 - 832   
      TA150970           833 - 835         TA150977           836 - 836         TA150978           837 - 837   
      TA150985           838 - 838         TA150986           839 - 839         TA150993           840 - 840   
      TA150994           841 - 841         TA150998           842 - 842         TA150999           843 - 843   
      TA151006           844 - 844         TA151007           845 - 845         TA151014           846 - 846   
      TA151015           847 - 847         TA151022           848 - 848         TA151023           849 - 849   
      TA151131           850 - 850         TA151132           851 - 851         TA151137           852 - 852   
      TA151138           853 - 853         TA151139           854 - 854         TA151140           855 - 855   
      TA151141           856 - 856         TA151142           857 - 857         TA151143           858 - 858   
      TA151144           859 - 859         TA151145           860 - 860         TA151146           861 - 861   
      TA151147           862 - 862         TA151148           863 - 863         TA151149           864 - 864   
      TA151150           865 - 865         TA151271           866 - 866         TA151272           867 - 867   
      TA151274           868 - 868         TA151276           869 - 869         TA151277           870 - 871   
      TA151283           872 - 875         TA151285           876 - 877         ER34501            878 - 882   
      ER34502            883 - 884         ER34503            885 - 886         TA170001           887 - 887   
      TA170016           888 - 888         TA170017           889 - 892         TA170018           893 - 893   
      TA170019           894 - 894         TA170020           895 - 895         TA170021           896 - 896   
      TA170022           897 - 897         TA170023           898 - 898         TA170024           899 - 899   
      TA170025           900 - 900         TA170026           901 - 902         TA170027           903 - 903   
      TA170028           904 - 904         TA170029           905 - 906         TA170030           907 - 908   
      TA170031           909 - 910         TA170032           911 - 912         TA170033           913 - 914   
      TA170034           915 - 916         TA170035           917 - 917         TA170058           918 - 919   
      TA170059           920 - 921         TA170061           922 - 922         TA170062           923 - 923   
      TA170063           924 - 924         TA170064           925 - 925         TA170065           926 - 926   
      TA170066           927 - 927         TA170067           928 - 928         TA170068           929 - 929   
      TA170069           930 - 930         TA170070           931 - 931         TA170071           932 - 932   
      TA170072           933 - 933         TA170073           934 - 934         TA170074           935 - 935   
      TA170075           936 - 936         TA170076           937 - 937         TA170077           938 - 938   
      TA170078           939 - 939         TA170093           940 - 940         TA170116           941 - 941   
      TA170176           942 - 943         TA170183           944 - 945         TA170187           946 - 946   
      TA170386           947 - 947         TA170416           948 - 948         TA170781           949 - 949   
      TA170790           950 - 950         TA170795           951 - 951         TA170866           952 - 952   
      TA170905           953 - 953         TA170907           954 - 954         TA170908           955 - 955   
      TA170909           956 - 956         TA170910           957 - 957         TA170911           958 - 958   
      TA170912           959 - 959         TA170913           960 - 960         TA170914           961 - 961   
      TA170923           962 - 962         TA171798           963 - 964         TA171806           965 - 965   
      TA171807           966 - 966         TA171809           967 - 969         TA171825           970 - 970   
      TA171826           971 - 972         TA171827           973 - 975         TA171834           976 - 976   
      TA171835           977 - 977         TA171839           978 - 978         TA171840           979 - 979   
      TA171852           980 - 980         TA171853           981 - 981         TA171860           982 - 982   
      TA171861           983 - 983         TA171868           984 - 984         TA171869           985 - 985   
      TA171876           986 - 986         TA171877           987 - 987         TA171884           988 - 988   
      TA171885           989 - 989         TA171955           990 - 991         TA171960           992 - 992   
      TA171971           993 - 993         TA171972           994 - 994         TA171974           995 - 995   
      TA171975           996 - 997         TA171978           998 - 1001        TA171980          1002 - 1003  
   .
   EXECUTE .
FORMATS 
      TA050944        (F4.1)  TA070925        (F4.1)  TA090989        (F4.1)  TA111131        (F4.1) 
      TA131223        (F4.1)  TA151283        (F4.1)  TA171978        (F4.1) 
.

VARIABLE LABELS
      TAS           "Sum of All TAS Flags"                    
      TAS05         "TAS2005 = 1 if exists, else missing"     
      TAS07         "TAS2007 = 1 if exists, else missing"     
      TAS09         "TAS2009 = 1 if exists, else missing"     
      TAS11         "TAS2011 = 1 if exists, else missing"     
      TAS13         "TAS2013 = 1 if exists, else missing"     
      TAS15         "TAS2015 = 1 if exists, else missing"     
      TAS17         "TAS2017 = 1 if exists, else missing"     
      ER30001       "1968 INTERVIEW NUMBER"                   
      ER30002       "PERSON NUMBER                         68"
      ER33801       "2005 INTERVIEW NUMBER"                   
      ER33802       "SEQUENCE NUMBER                       05"
      ER33803       "RELATION TO HEAD                      05"
      TA050001      "RELEASE NUMBER"                          
      TA050015      "A1 WTR INVOLVED IN ARTS"                 
      TA050016      "A2 TYPE OF ARTS"                         
      TA050017      "A3 HOW OFTEN PARTICIPATED IN ARTS"       
      TA050018      "A4 WTR MEMBER OF SPORTS TEAM"            
      TA050019      "A5 HOW OFTEN PARTICIPATD IN SPORTS"      
      TA050020      "A6 HOW OFTEN WATCHED OR READ NEWS"       
      TA050021      "A7 HOW OFTEN READ FOR PLEASURE"          
      TA050022      "A8 HOW OFTEN WATCH NON-NEWS TV"          
      TA050023      "A9 WTR EVER USED INTERNET"               
      TA050024      "A10A WTR USED INTERNET FOR EMAIL"        
      TA050025      "A10B WTR USED INTERNET FOR SCHOOL"       
      TA050026      "A10C WTR USED INTERNET TO SHOP"          
      TA050027      "A10D WTR USED INTERNET FOR GAMES"        
      TA050028      "A11 WTR VOTED IN 2004"                   
      TA050029      "A11B WTR IN SOCIAL ACTION GROUPS"        
      TA050030      "A11C TYPE OF SOCIAL ACTION GROUPS"       
      TA050031      "A11D HOW OFTEN DID SOCIAL ACTION GROUPS" 
      TA050032      "A12 WTR INVOLVED WITH SCHOOL CLUBS"      
      TA050033      "A12B HOW OFTEN DID SCHOOL CLUBS"         
      TA050034      "A13 WTR DID OTR VOLUNTEER WORK"          
      TA050035      "A14 TYPE VOLUNTEER ORG-MENTION 1"        
      TA050036      "A14 TYPE VOLUNTEER ORG-MENTION 2"        
      TA050037      "A14 TYPE VOLUNTEER ORG-MENTION 3"        
      TA050038      "A14 TYPE VOLUNTEER ORG-MENTION 4"        
      TA050039      "A14 TYPE VOLUNTEER ORG-MENTION 5"        
      TA050040      "A14 TYPE VOLUNTEER ORG-MENTION 6"        
      TA050041      "A14B HOW OFTEN VOLUNTEERED"              
      TA050042      "B1 FALL/WINTER PRIMARY RESIDENCE"        
      TA050043      "B2 SUMMER PRIMARY RESIDENCE"             
      TA050044      "B5A HOW MUCH RESONSIBLTY EARNG OWN LIVNG"
      TA050045      "B5B HOW MUCH RESPONSIBLTY PAYNG OWN RENT"
      TA050046      "B5C HOW MUCH RESPONSBLTY FOR OWN BILLS"  
      TA050047      "B5D HOW MUCH RESPONSIBLTY MANAGING MONEY"
      TA050048      "B6A HOW GOOD AT RESPONSIBILITY"          
      TA050049      "B6B HOW GOOD AT PROBLEM SOLVING"         
      TA050050      "B6C HOW GOOD AT MONEY MANAGEMENT"        
      TA050051      "B6D HOW GOOD AT CREDIT CARD PAYOFF"      
      TA050052      "C1A HOW GOOD AT SUPERVISING COMP"        
      TA050053      "C1B HOW GOOD AT LEADING COMP W/OTRS"     
      TA050054      "C1C HOW GOOD AT LOGIC COMP W/OTRS"       
      TA050055      "C1D HOW GOOD AT HELPING COMP W/OTRS"     
      TA050056      "C1E HOW INTELLIGENT COMPARED W/OTRS"     
      TA050057      "C1F HOW INDEPENDENT COMPARED W/OTRS"     
      TA050058      "C1G HOW CONFIDENT COMPARED W/OTHERS"     
      TA050059      "C1H HOW DECISIVE COMPARED W/OTHERS"      
      TA050060      "C1J HOW WELL LISTEN COMPARED W/OTHERS"   
      TA050061      "C1K HOW GOOD AT TEACHING COMPARED W/OTRS"
      TA050069      "D1 CURRENT MARITAL STATUS"               
      TA050078      "D8 WTR ROMANTIC RELATIONSHIP NOW"        
      TA050091      "D28A NUMBER OF CHILDREN"                 
      TA050127      "E1 EMPLOYMENT STATUS 1ST MENTION"        
      TA050131      "E3A WTR WORKD SINCE JAN 1 OF PRIOR YEAR" 
      TA050368      "E62 WTR EVER WORKED"                     
      TA050394      "E70 WHY NOT LOOKNG FOR WRK IN LAST 4 WKS"
      TA050573      "G1 WTR GRADUATED HIGH SCHOOL"            
      TA050594      "G10 WTR EVER ATTENDED COLLEGE"           
      TA050595      "G11 WTR IN COLLEGE NOW"                  
      TA050631      "G20 WTR CURRENTLY IN VO/TECH TRAINING"   
      TA050678      "H3 HOW MUCH CONDITION LIMITS AMT OF WORK"
      TA050708      "H12 WTR EVER HAD EMOTIONAL/PSYCH PROBS"  
      TA050710      "H12B WTR DEPRESSION"                     
      TA050711      "H12B WTR BIPOLAR DISORDER"               
      TA050712      "H12B WTR SCHIZOPHRENIA"                  
      TA050713      "H12B WTR ANXIETY"                        
      TA050714      "H12B WTR PHOBIAS"                        
      TA050715      "H12B WTR ALCOHOL PROBLEMS"               
      TA050716      "H12B WTR OTHER DRUG PROBLEMS"            
      TA050717      "H12B WTR OBSESSIVE COMPULSIVE DISORDER"  
      TA050723      "H13B HOW MUCH LIMITS NORMAL ACTIVITIES"  
      TA050733      "H15 WTR>2 WKS DEPRESSED IN PAST 12 MOS"  
      TA050734      "H16 WTR>2 WKS NO INTEREST IN LIFE"       
      TA050754      "H27 # OF HOURS OF SLEEP IN 24-HR PERIOD" 
      TA050755      "H28A FREQ OF SNACK INSTEAD OF REGLR MEAL"
      TA050756      "H28B FREQUENCY OF BINGE EATING"          
      TA050759      "H30 # CIGARETTES PER DAY"                
      TA050767      "H37 HOW OFTEN HAVE DRINKS-HD"            
      TA050768      "H38 # ALCOHOLIC DRINKS PER DAY-HD"       
      TA050769      "H39 # DAYS HAD 4-5 DRINKS-HEAD"          
      TA050776      "H44A # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA050777      "H45A # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA050784      "H44B # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA050785      "H45B # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA050792      "H44C # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA050793      "H45C # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA050797      "H42D_B # OF OCCASIONS IN PAST 12 MOS"    
      TA050798      "H42D_C # OF OCCASIONS IN PAST 30 DAYS"   
      TA050808      "H44E # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA050809      "H45E # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA050816      "H44F # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA050817      "H45F # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA050824      "H44G # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA050825      "H45G # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA050883      "L6 HISPANICITY"                          
      TA050884      "L7 RACE MENTION #1"                      
      TA050888      "M1 FREQUENCY OF HAPPINESS IN LAST MONTH" 
      TA050889      "M2 FREQ OF INTEREST IN LIFE IN LAST MO"  
      TA050890      "M3 FREQ OF FEELING SATISFIED IN LAST MO" 
      TA050891      "M4 FREQ OF FEELING CONTRIB TO SOCIETY"   
      TA050892      "M5 FREQ OF FEELING BELONGING TO COMMUNTY"
      TA050893      "M6 FREQ OF FEELING SOCIETY GETTNG BETTER"
      TA050894      "M7 FREQ OF FEELING PEOPLE BASICALLY GOOD"
      TA050895      "M8 FREQ FEELNG WAY SOC WORKS MAKES SENSE"
      TA050896      "M9 FREQ FEEL MANAGNG DAILY RESPONSIBILTY"
      TA050897      "M10 FREQ FEELING TRUSTING RELS W/OTHERS" 
      TA050898      "M11 FREQ OF FEELING CHALLENGED TO GROW"  
      TA050899      "M12 FREQ FEELING CONFIDENT OF OWN IDEAS" 
      TA050900      "M13 FREQ OF FEELING LIKED PERSONALITY"   
      TA050901      "M14 FREQ OF FEELING LIFE HAD DIRECTION"  
      TA050932      "MENTAL HEALTH:  WORRY"                   
      TA050933      "MENTAL HEALTH:  SOCIAL ANXIETY"          
      TA050935      "SUBSCALE:  EMOTIONAL WB"                 
      TA050937      "SUBSCALE:  PSYCHOLOGICAL WB"             
      TA050938      "MENTAL HEALTH:  NON-SPEC PSYCH DISTRESS" 
      TA050944      "BODY MASS INDEX"                         
      TA050946      "ENROLLMENT STATUS"                       
      ER33901       "2007 INTERVIEW NUMBER"                   
      ER33902       "SEQUENCE NUMBER                       07"
      ER33903       "RELATION TO HEAD                      07"
      TA070001      "RELEASE NUMBER"                          
      TA070015      "A1 WTR INVOLVED IN ARTS"                 
      TA070016      "A2 TYPE OF ARTS"                         
      TA070017      "A3 HOW OFTEN PARTICIPATED IN ARTS"       
      TA070018      "A4 WTR MEMBER OF SPORTS TEAM"            
      TA070019      "A5 HOW OFTEN PARTICIPATD IN SPORTS"      
      TA070020      "A6 HOW OFTEN WATCHED OR READ NEWS"       
      TA070021      "A7 HOW OFTEN READ FOR PLEASURE"          
      TA070022      "A8 HOW OFTEN WATCH NON-NEWS TV"          
      TA070023      "A9 WTR EVER USED INTERNET"               
      TA070024      "A10A WTR USED INTERNET FOR EMAIL"        
      TA070025      "A10B WTR USED INTERNET FOR SCHOOL"       
      TA070026      "A10C WTR USED INTERNET TO SHOP"          
      TA070027      "A10D WTR USED INTERNET FOR GAMES"        
      TA070028      "A11 WTR VOTED IN 2006"                   
      TA070029      "A11B WTR IN SOCIAL ACTION GROUPS"        
      TA070030      "A11C TYPE OF SOCIAL ACTION GROUPS"       
      TA070031      "A11D HOW OFTEN DID SOCIAL ACTION GROUPS" 
      TA070032      "A12 WTR INVOLVED WITH SCHOOL CLUBS"      
      TA070033      "A12B HOW OFTEN DID SCHOOL CLUBS"         
      TA070034      "A13 WTR DID OTR VOLUNTEER WORK"          
      TA070035      "A14 TYPE VOLUNTEER ORG-MENTION 1"        
      TA070036      "A14 TYPE VOLUNTEER ORG-MENTION 2"        
      TA070037      "A14 TYPE VOLUNTEER ORG-MENTION 3"        
      TA070038      "A14 TYPE VOLUNTEER ORG-MENTION 4"        
      TA070039      "A14 TYPE VOLUNTEER ORG-MENTION 5"        
      TA070040      "A14 TYPE VOLUNTEER ORG-MENTION 6"        
      TA070041      "A14B HOW OFTEN VOLUNTEERED"              
      TA070042      "B1 FALL/WINTER PRIMARY RESIDENCE"        
      TA070043      "B2 SUMMER PRIMARY RESIDENCE"             
      TA070044      "B5A HOW MUCH RESONSIBLTY EARNG OWN LIVNG"
      TA070045      "B5B HOW MUCH RESPONSIBLTY PAYNG OWN RENT"
      TA070046      "B5C HOW MUCH RESPONSBLTY FOR OWN BILLS"  
      TA070047      "B5D HOW MUCH RESPONSIBLTY MANAGING MONEY"
      TA070048      "B6A HOW GOOD AT RESPONSIBILITY"          
      TA070049      "B6B HOW GOOD AT PROBLEM SOLVING"         
      TA070050      "B6C HOW GOOD AT MONEY MANAGEMENT"        
      TA070051      "B6D HOW GOOD AT CREDIT CARD PAYOFF"      
      TA070052      "C1A HOW GOOD AT SUPERVISING COMP"        
      TA070053      "C1B HOW GOOD AT LEADING COMP W/OTRS"     
      TA070054      "C1C HOW GOOD AT LOGIC COMP W/OTRS"       
      TA070055      "C1D HOW GOOD AT HELPING COMP W/OTRS"     
      TA070056      "C1E HOW INTELLIGENT COMPARED W/OTRS"     
      TA070057      "C1F HOW INDEPENDENT COMPARED W/OTRS"     
      TA070058      "C1G HOW CONFIDENT COMPARED W/OTHERS"     
      TA070059      "C1H HOW DECISIVE COMPARED W/OTHERS"      
      TA070060      "C1J HOW WELL LISTEN COMPARED W/OTHERS"   
      TA070061      "C1K HOW GOOD AT TEACHING COMPARED W/OTRS"
      TA070069      "D1 CURRENT MARITAL STATUS"               
      TA070078      "D8 WTR ROMANTIC RELATIONSHIP NOW"        
      TA070091      "D28A NUMBER OF CHILDREN"                 
      TA070127      "E1 EMPLOYMENT STATUS 1ST MENTION"        
      TA070131      "E3A WTR WORKD SINCE JAN 1 OF PRIOR YEAR" 
      TA070341      "E62 WTR EVER WORKED"                     
      TA070368      "E70 WHY NOT LOOKNG FOR WRK IN LAST 4 WKS"
      TA070548      "G1 WTR GRADUATED HIGH SCHOOL"            
      TA070569      "G10 WTR EVER ATTENDED COLLEGE"           
      TA070570      "G11 WTR IN COLLEGE NOW"                  
      TA070602      "G20 WTR CURRENTLY IN VO/TECH TRAINING"   
      TA070649      "H3 HOW MUCH CONDITION LIMITS AMT OF WORK"
      TA070679      "H12 WTR EVER HAD EMOTIONAL/PSYCH PROBS"  
      TA070681      "H12B WTR DEPRESSION"                     
      TA070682      "H12B WTR BIPOLAR DISORDER"               
      TA070683      "H12B WTR SCHIZOPHRENIA"                  
      TA070684      "H12B WTR ANXIETY"                        
      TA070685      "H12B WTR PHOBIAS"                        
      TA070686      "H12B WTR ALCOHOL PROBLEMS"               
      TA070687      "H12B WTR OTHER DRUG PROBLEMS"            
      TA070688      "H12B WTR OBSESSIVE COMPULSIVE DISORDER"  
      TA070694      "H13B HOW MUCH LIMITS NORMAL ACTIVITIES"  
      TA070704      "H15 WTR>2 WKS DEPRESSED IN PAST 12 MOS"  
      TA070705      "H16 WTR>2 WKS NO INTEREST IN LIFE"       
      TA070725      "H27 # OF HOURS OF SLEEP IN 24-HR PERIOD" 
      TA070726      "H28A FREQ OF SNACK INSTEAD OF REGLR MEAL"
      TA070727      "H28B FREQUENCY OF BINGE EATING"          
      TA070730      "H30 # CIGARETTES PER DAY"                
      TA070738      "H37 HOW OFTEN HAVE DRINKS-HD"            
      TA070739      "H38 # ALCOHOLIC DRINKS PER DAY-HD"       
      TA070740      "H39 # DAYS HAD 4-5 DRINKS-HEAD"          
      TA070747      "H44A # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA070748      "H45A # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA070755      "H44B # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA070756      "H45B # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA070763      "H44C # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA070764      "H45C # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA070768      "H42D_B # OF OCCASIONS IN PAST 12 MOS"    
      TA070769      "H42D_C # OF OCCASIONS IN PAST 30 DAYS"   
      TA070776      "H44E # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA070777      "H45E # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA070784      "H44F # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA070785      "H45F # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA070792      "H44G # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA070793      "H45G # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA070864      "L6 HISPANICITY"                          
      TA070865      "L7 RACE MENTION #1"                      
      TA070869      "M1 FREQUENCY OF HAPPINESS IN LAST MONTH" 
      TA070870      "M2 FREQ OF INTEREST IN LIFE IN LAST MO"  
      TA070871      "M3 FREQ OF FEELING SATISFIED IN LAST MO" 
      TA070872      "M4 FREQ OF FEELING CONTRIB TO SOCIETY"   
      TA070873      "M5 FREQ OF FEELING BELONGING TO COMMUNTY"
      TA070874      "M6 FREQ OF FEELING SOCIETY GETTNG BETTER"
      TA070875      "M7 FREQ OF FEELING PEOPLE BASICALLY GOOD"
      TA070876      "M8 FREQ FEELNG WAY SOC WORKS MAKES SENSE"
      TA070877      "M9 FREQ FEEL MANAGNG DAILY RESPONSIBILTY"
      TA070878      "M10 FREQ FEELING TRUSTING RELS W/OTHERS" 
      TA070879      "M11 FREQ OF FEELING CHALLENGED TO GROW"  
      TA070880      "M12 FREQ FEELING CONFIDENT OF OWN IDEAS" 
      TA070881      "M13 FREQ OF FEELING LIKED PERSONALITY"   
      TA070882      "M14 FREQ OF FEELING LIFE HAD DIRECTION"  
      TA070913      "MENTAL HEALTH:  WORRY"                   
      TA070914      "MENTAL HEALTH:  SOCIAL ANXIETY"          
      TA070916      "SUBSCALE:  EMOTIONAL WB"                 
      TA070918      "SUBSCALE:  PSYCHOLOGICAL WB"             
      TA070919      "MENTAL HEALTH:  NON-SPEC PSYCH DISTRESS" 
      TA070925      "BODY MASS INDEX"                         
      TA070927      "ENROLLMENT STATUS"                       
      ER34001       "2009 INTERVIEW NUMBER"                   
      ER34002       "SEQUENCE NUMBER                       09"
      ER34003       "RELATION TO HEAD                      09"
      TA090001      "RELEASE NUMBER"                          
      TA090016      "A1 WTR INVOLVED IN ARTS"                 
      TA090017      "A2 TYPE OF ARTS"                         
      TA090018      "A3 HOW OFTEN PARTICIPATED IN ARTS"       
      TA090019      "A4 WTR MEMBER OF SPORTS TEAM"            
      TA090020      "A5 HOW OFTEN PARTICIPATD IN SPORTS"      
      TA090021      "A6 HOW OFTEN WATCHED OR READ NEWS"       
      TA090022      "A7 HOW OFTEN READ FOR PLEASURE"          
      TA090023      "A8 HOW OFTEN WATCH NON-NEWS TV"          
      TA090024      "A9 WTR EVER USED INTERNET"               
      TA090025      "A10A WTR USED INTERNET FOR EMAIL"        
      TA090026      "A10B WTR USED INTERNET FOR SCHOOL"       
      TA090027      "A10C WTR USED INTERNET TO SHOP"          
      TA090028      "A10D WTR USED INTERNET FOR GAMES"        
      TA090029      "A11 WTR VOTED IN 2006"                   
      TA090030      "A11B WTR IN SOCIAL ACTION GROUPS"        
      TA090031      "A11C TYPE OF SOCIAL ACTION GROUPS"       
      TA090032      "A11D HOW OFTEN DID SOCIAL ACTION GROUPS" 
      TA090033      "A12 WTR INVOLVED WITH SCHOOL CLUBS"      
      TA090034      "A12B HOW OFTEN DID SCHOOL CLUBS"         
      TA090035      "A13 WTR DID OTR VOLUNTEER WORK"          
      TA090036      "A14 TYPE VOLUNTEER ORG-MENTION 1"        
      TA090037      "A14 TYPE VOLUNTEER ORG-MENTION 2"        
      TA090038      "A14 TYPE VOLUNTEER ORG-MENTION 3"        
      TA090039      "A14 TYPE VOLUNTEER ORG-MENTION 4"        
      TA090040      "A14 TYPE VOLUNTEER ORG-MENTION 5"        
      TA090041      "A14 TYPE VOLUNTEER ORG-MENTION 6"        
      TA090042      "A14B HOW OFTEN VOLUNTEERED"              
      TA090043      "B1 FALL/WINTER PRIMARY RESIDENCE"        
      TA090044      "B2 SUMMER PRIMARY RESIDENCE"             
      TA090045      "B5A HOW MUCH RESONSIBLTY EARNG OWN LIVNG"
      TA090046      "B5B HOW MUCH RESPONSIBLTY PAYNG OWN RENT"
      TA090047      "B5C HOW MUCH RESPONSBLTY FOR OWN BILLS"  
      TA090048      "B5D HOW MUCH RESPONSIBLTY MANAGING MONEY"
      TA090049      "B6A HOW GOOD AT RESPONSIBILITY"          
      TA090050      "B6B HOW GOOD AT PROBLEM SOLVING"         
      TA090051      "B6C HOW GOOD AT MONEY MANAGEMENT"        
      TA090052      "B6D HOW GOOD AT CREDIT CARD PAYOFF"      
      TA090053      "C1A HOW GOOD AT SUPERVISING COMP"        
      TA090054      "C1B HOW GOOD AT LEADING COMP W/OTRS"     
      TA090055      "C1C HOW GOOD AT LOGIC COMP W/OTRS"       
      TA090056      "C1D HOW GOOD AT HELPING COMP W/OTRS"     
      TA090057      "C1E HOW INTELLIGENT COMPARED W/OTRS"     
      TA090058      "C1F HOW INDEPENDENT COMPARED W/OTRS"     
      TA090059      "C1G HOW CONFIDENT COMPARED W/OTHERS"     
      TA090060      "C1H HOW DECISIVE COMPARED W/OTHERS"      
      TA090061      "C1J HOW WELL LISTEN COMPARED W/OTHERS"   
      TA090062      "C1K HOW GOOD AT TEACHING COMPARED W/OTRS"
      TA090078      "D1 CURRENT MARITAL STATUS"               
      TA090087      "D8 WTR ROMANTIC RELATIONSHIP NOW"        
      TA090100      "D28A NUMBER OF CHILDREN"                 
      TA090136      "E1 EMPLOYMENT STATUS 1ST MENTION"        
      TA090140      "E3A WTR WORKD SINCE JAN 1 OF PRIOR YEAR" 
      TA090358      "E62 WTR EVER WORKED"                     
      TA090385      "E70 WHY NOT LOOKNG FOR WRK IN LAST 4 WKS"
      TA090590      "G1 WTR GRADUATED HIGH SCHOOL"            
      TA090611      "G10 WTR EVER ATTENDED COLLEGE"           
      TA090612      "G11 WTR IN COLLEGE NOW"                  
      TA090655      "G20 WTR CURRENTLY IN VO/TECH TRAINING"   
      TA090705      "H3 HOW MUCH CONDITION LIMITS AMT OF WORK"
      TA090735      "H12 WTR EVER HAD EMOTIONAL/PSYCH PROBS"  
      TA090737      "H12B WTR DEPRESSION"                     
      TA090738      "H12B WTR BIPOLAR DISORDER"               
      TA090739      "H12B WTR SCHIZOPHRENIA"                  
      TA090740      "H12B WTR ANXIETY"                        
      TA090741      "H12B WTR PHOBIAS"                        
      TA090742      "H12B WTR ALCOHOL PROBLEMS"               
      TA090743      "H12B WTR OTHER DRUG PROBLEMS"            
      TA090744      "H12B WTR OBSESSIVE COMPULSIVE DISORDER"  
      TA090750      "H13B HOW MUCH LIMITS NORMAL ACTIVITIES"  
      TA090760      "H15 WTR>2 WKS DEPRESSED IN PAST 12 MOS"  
      TA090761      "H16 WTR>2 WKS NO INTEREST IN LIFE"       
      TA090784      "H27 # OF HOURS OF SLEEP IN 24-HR PERIOD" 
      TA090785      "H28A FREQ OF SNACK INSTEAD OF REGLR MEAL"
      TA090786      "H28B FREQUENCY OF BINGE EATING"          
      TA090789      "H30 # CIGARETTES PER DAY"                
      TA090797      "H37 HOW OFTEN HAVE DRINKS-HD"            
      TA090798      "H38 # ALCOHOLIC DRINKS PER DAY-HD"       
      TA090799      "H39 # DAYS HAD 4-5 DRINKS-HEAD"          
      TA090806      "H44A # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA090807      "H45A # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA090814      "H44B # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA090815      "H45B # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA090822      "H44C # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA090823      "H45C # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA090827      "H42D_B # OF OCCASIONS IN PAST 12 MOS"    
      TA090828      "H42D_C # OF OCCASIONS IN PAST 30 DAYS"   
      TA090835      "H44E # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA090836      "H45E # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA090843      "H44F # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA090844      "H45F # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA090851      "H44G # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA090852      "H45G # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA090924      "L6 HISPANICITY"                          
      TA090925      "L7 RACE MENTION #1"                      
      TA090929      "M1 FREQUENCY OF HAPPINESS IN LAST MONTH" 
      TA090930      "M2 FREQ OF INTEREST IN LIFE IN LAST MO"  
      TA090931      "M3 FREQ OF FEELING SATISFIED IN LAST MO" 
      TA090932      "M4 FREQ OF FEELING CONTRIB TO SOCIETY"   
      TA090933      "M5 FREQ OF FEELING BELONGING TO COMMUNTY"
      TA090934      "M6 FREQ OF FEELING SOCIETY GETTNG BETTER"
      TA090935      "M7 FREQ OF FEELING PEOPLE BASICALLY GOOD"
      TA090936      "M8 FREQ FEELNG WAY SOC WORKS MAKES SENSE"
      TA090937      "M9 FREQ FEEL MANAGNG DAILY RESPONSIBILTY"
      TA090938      "M10 FREQ FEELING TRUSTING RELS W/OTHERS" 
      TA090939      "M11 FREQ OF FEELING CHALLENGED TO GROW"  
      TA090940      "M12 FREQ FEELING CONFIDENT OF OWN IDEAS" 
      TA090941      "M13 FREQ OF FEELING LIKED PERSONALITY"   
      TA090942      "M14 FREQ OF FEELING LIFE HAD DIRECTION"  
      TA090977      "MENTAL HEALTH:  WORRY"                   
      TA090978      "MENTAL HEALTH:  SOCIAL ANXIETY"          
      TA090980      "SUBSCALE:  EMOTIONAL WB"                 
      TA090982      "SUBSCALE:  PSYCHOLOGICAL WB"             
      TA090983      "MENTAL HEALTH:  NON-SPEC PSYCH DISTRESS" 
      TA090989      "BODY MASS INDEX"                         
      TA090991      "ENROLLMENT STATUS"                       
      ER34101       "2011 INTERVIEW NUMBER"                   
      ER34102       "SEQUENCE NUMBER                       11"
      ER34103       "RELATION TO HEAD                      11"
      TA110001      "RELEASE NUMBER"                          
      TA110016      "A1 WTR INVOLVED IN ARTS"                 
      TA110017      "A2 TYPE OF ARTS"                         
      TA110018      "A3 HOW OFTEN PARTICIPATED IN ARTS"       
      TA110019      "A4 WTR MEMBER OF SPORTS TEAM"            
      TA110020      "A5 HOW OFTEN PARTICIPATD IN SPORTS"      
      TA110021      "A6 HOW OFTEN WATCHED OR READ NEWS"       
      TA110022      "A7 HOW OFTEN READ FOR PLEASURE"          
      TA110023      "A8 HOW OFTEN WATCH NON-NEWS TV"          
      TA110024      "A9 WTR EVER USED INTERNET"               
      TA110025      "A10A WTR USED INTERNET FOR EMAIL"        
      TA110026      "A10B WTR USED INTERNET FOR SCHOOL"       
      TA110027      "A10C WTR USED INTERNET TO SHOP"          
      TA110028      "A10D WTR USED INTERNET FOR GAMES"        
      TA110029      "A10E WTR USED INTERNET FOR SOC NETWRKING"
      TA110030      "A11 WTR VOTED IN 2010"                   
      TA110031      "A11B WTR IN SOCIAL ACTION GROUPS"        
      TA110032      "A11C TYPE OF SOCIAL ACTION GROUPS"       
      TA110033      "A11D HOW OFTEN DID SOCIAL ACTION GROUPS" 
      TA110034      "A12 WTR INVOLVED WITH SCHOOL CLUBS"      
      TA110035      "A12B HOW OFTEN DID SCHOOL CLUBS"         
      TA110036      "A13 WTR DID OTR VOLUNTEER WORK"          
      TA110037      "A14 TYPE VOLUNTEER ORG-MENTION 1"        
      TA110038      "A14 TYPE VOLUNTEER ORG-MENTION 2"        
      TA110039      "A14 TYPE VOLUNTEER ORG-MENTION 3"        
      TA110040      "A14 TYPE VOLUNTEER ORG-MENTION 4"        
      TA110041      "A14 TYPE VOLUNTEER ORG-MENTION 5"        
      TA110042      "A14 TYPE VOLUNTEER ORG-MENTION 6"        
      TA110043      "A14B HOW OFTEN VOLUNTEERED"              
      TA110044      "B1 FALL/WINTER PRIMARY RESIDENCE"        
      TA110045      "B2 SUMMER PRIMARY RESIDENCE"             
      TA110046      "B5A HOW MUCH RESONSIBLTY EARNG OWN LIVNG"
      TA110047      "B5B HOW MUCH RESPONSIBLTY PAYNG OWN RENT"
      TA110048      "B5C HOW MUCH RESPONSBLTY FOR OWN BILLS"  
      TA110049      "B5D HOW MUCH RESPONSIBLTY MANAGING MONEY"
      TA110050      "B6A HOW GOOD AT RESPONSIBILITY"          
      TA110051      "B6B HOW GOOD AT PROBLEM SOLVING"         
      TA110052      "B6C HOW GOOD AT MONEY MANAGEMENT"        
      TA110053      "B6D HOW GOOD AT CREDIT CARD PAYOFF"      
      TA110054      "C1A HOW GOOD AT SUPERVISING COMP"        
      TA110055      "C1B HOW GOOD AT LEADING COMP W/OTRS"     
      TA110056      "C1C HOW GOOD AT LOGIC COMP W/OTRS"       
      TA110057      "C1D HOW GOOD AT HELPING COMP W/OTRS"     
      TA110058      "C1E HOW INTELLIGENT COMPARED W/OTRS"     
      TA110059      "C1F HOW INDEPENDENT COMPARED W/OTRS"     
      TA110060      "C1G HOW CONFIDENT COMPARED W/OTHERS"     
      TA110061      "C1H HOW DECISIVE COMPARED W/OTHERS"      
      TA110062      "C1J HOW WELL LISTEN COMPARED W/OTHERS"   
      TA110063      "C1K HOW GOOD AT TEACHING COMPARED W/OTRS"
      TA110079      "D1 CURRENT MARITAL STATUS"               
      TA110088      "D8 WTR ROMANTIC RELATIONSHIP NOW"        
      TA110101      "D28A NUMBER OF CHILDREN"                 
      TA110137      "E1 EMPLOYMENT STATUS 1ST MENTION"        
      TA110141      "E3A WTR WORKD SINCE JAN 1 OF PRIOR YEAR" 
      TA110348      "E62 WTR EVER WORKED"                     
      TA110462      "E70 WHY NOT LOOKNG FOR WRK IN LAST 4 WKS"
      TA110671      "G1 WTR GRADUATED HIGH SCHOOL"            
      TA110698      "G10 WTR EVER ATTENDED COLLEGE"           
      TA110699      "G11 WTR IN COLLEGE NOW"                  
      TA110743      "G20 WTR CURRENTLY IN VO/TECH TRAINING"   
      TA110793      "H3 HOW MUCH CONDITION LIMITS AMT OF WORK"
      TA110825      "H12 WTR EVER HAD EMOTIONAL/PSYCH PROBS"  
      TA110827      "H12B WTR DEPRESSION"                     
      TA110828      "H12B WTR BIPOLAR DISORDER"               
      TA110829      "H12B WTR SCHIZOPHRENIA"                  
      TA110830      "H12B WTR ANXIETY"                        
      TA110831      "H12B WTR PHOBIAS"                        
      TA110832      "H12B WTR ALCOHOL PROBLEMS"               
      TA110833      "H12B WTR OTHER DRUG PROBLEMS"            
      TA110834      "H12B WTR OBSESSIVE COMPULSIVE DISORDER"  
      TA110842      "H13B HOW MUCH LIMITS NORMAL ACTIVITIES"  
      TA110853      "H15 WTR>2 WKS DEPRESSED IN PAST 12 MOS"  
      TA110854      "H16 WTR>2 WKS NO INTEREST IN LIFE"       
      TA110900      "H27 # OF HOURS OF SLEEP IN 24-HR PERIOD" 
      TA110901      "H28A FREQ OF SNACK INSTEAD OF REGLR MEAL"
      TA110902      "H28B FREQUENCY OF BINGE EATING"          
      TA110905      "H30 # CIGARETTES PER DAY"                
      TA110913      "H37 HOW OFTEN HAVE DRINKS-HD"            
      TA110914      "H38 # ALCOHOLIC DRINKS PER DAY-HD"       
      TA110915      "H39 # DAYS HAD 4-5 DRINKS-HEAD"          
      TA110922      "H44A # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA110923      "H45A # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA110930      "H44B # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA110931      "H45B # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA110938      "H44C # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA110939      "H45C # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA110943      "H42D_B # OF OCCASIONS IN PAST 12 MOS"    
      TA110944      "H42D_C # OF OCCASIONS IN PAST 30 DAYS"   
      TA110951      "H44E # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA110952      "H45E # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA110959      "H44F # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA110960      "H45F # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA110967      "H44G # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA110968      "H45G # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA111056      "L6 HISPANICITY"                          
      TA111057      "L7 RACE MENTION #1"                      
      TA111061      "M1 FREQUENCY OF HAPPINESS IN LAST MONTH" 
      TA111062      "M2 FREQ OF INTEREST IN LIFE IN LAST MO"  
      TA111063      "M3 FREQ OF FEELING SATISFIED IN LAST MO" 
      TA111064      "M4 FREQ OF FEELING CONTRIB TO SOCIETY"   
      TA111065      "M5 FREQ OF FEELING BELONGING TO COMMUNTY"
      TA111066      "M6 FREQ OF FEELING SOCIETY GETTNG BETTER"
      TA111067      "M7 FREQ OF FEELING PEOPLE BASICALLY GOOD"
      TA111068      "M8 FREQ FEELNG WAY SOC WORKS MAKES SENSE"
      TA111069      "M9 FREQ FEEL MANAGNG DAILY RESPONSIBILTY"
      TA111070      "M10 FREQ FEELING TRUSTING RELS W/OTHERS" 
      TA111071      "M11 FREQ OF FEELING CHALLENGED TO GROW"  
      TA111072      "M12 FREQ FEELING CONFIDENT OF OWN IDEAS" 
      TA111073      "M13 FREQ OF FEELING LIKED PERSONALITY"   
      TA111074      "M14 FREQ OF FEELING LIFE HAD DIRECTION"  
      TA111119      "MENTAL HEALTH:  WORRY"                   
      TA111120      "MENTAL HEALTH:  SOCIAL ANXIETY"          
      TA111122      "SUBSCALE:  EMOTIONAL WB"                 
      TA111124      "SUBSCALE:  PSYCHOLOGICAL WB"             
      TA111125      "MENTAL HEALTH:  NON-SPEC PSYCH DISTRESS" 
      TA111131      "BODY MASS INDEX"                         
      TA111133      "ENROLLMENT STATUS"                       
      ER34201       "2013 INTERVIEW NUMBER"                   
      ER34202       "SEQUENCE NUMBER                       13"
      ER34203       "RELATION TO HEAD                      13"
      TA130001      "RELEASE NUMBER"                          
      TA130016      "A1 WTR INVOLVED IN ARTS"                 
      TA130017      "A2 TYPE OF ARTS"                         
      TA130018      "A3 HOW OFTEN PARTICIPATED IN ARTS"       
      TA130019      "A4 WTR MEMBER OF SPORTS TEAM"            
      TA130020      "A5 HOW OFTEN PARTICIPATD IN SPORTS"      
      TA130021      "A6 HOW OFTEN WATCHED OR READ NEWS"       
      TA130022      "A7 HOW OFTEN READ FOR PLEASURE"          
      TA130023      "A8 HOW OFTEN WATCH NON-NEWS TV"          
      TA130024      "A10A WTR USED INTERNET FOR EMAIL"        
      TA130025      "A10B WTR USED INTERNET FOR SCHOOL"       
      TA130026      "A10C WTR USED INTERNET TO SHOP"          
      TA130027      "A10D WTR USED INTERNET FOR GAMES"        
      TA130028      "A10E WTR USED INTERNET FOR SOC NETWRKING"
      TA130029      "A11 WTR VOTED IN 2012"                   
      TA130030      "A11B WTR IN SOCIAL ACTION GROUPS"        
      TA130031      "A11C TYPE OF SOCIAL ACTION GROUPS"       
      TA130032      "A11D HOW OFTEN DID SOCIAL ACTION GROUPS" 
      TA130033      "A12 WTR INVOLVED WITH SCHOOL CLUBS"      
      TA130034      "A12B HOW OFTEN DID SCHOOL CLUBS"         
      TA130035      "A13 WTR DID OTR VOLUNTEER WORK"          
      TA130036      "A14 TYPE VOLUNTEER ORG-MENTION 1"        
      TA130037      "A14 TYPE VOLUNTEER ORG-MENTION 2"        
      TA130038      "A14 TYPE VOLUNTEER ORG-MENTION 3"        
      TA130039      "A14 TYPE VOLUNTEER ORG-MENTION 4"        
      TA130040      "A14 TYPE VOLUNTEER ORG-MENTION 5"        
      TA130041      "A14 TYPE VOLUNTEER ORG-MENTION 6"        
      TA130042      "A14B HOW OFTEN VOLUNTEERED"              
      TA130043      "B1 FALL/WINTER PRIMARY RESIDENCE"        
      TA130044      "B2 SUMMER PRIMARY RESIDENCE"             
      TA130045      "B5A HOW MUCH RESONSIBLTY EARNG OWN LIVNG"
      TA130046      "B5B HOW MUCH RESPONSIBLTY PAYNG OWN RENT"
      TA130047      "B5C HOW MUCH RESPONSBLTY FOR OWN BILLS"  
      TA130048      "B5D HOW MUCH RESPONSIBLTY MANAGING MONEY"
      TA130049      "B6A HOW GOOD AT RESPONSIBILITY"          
      TA130050      "B6B HOW GOOD AT PROBLEM SOLVING"         
      TA130051      "B6C HOW GOOD AT MONEY MANAGEMENT"        
      TA130052      "B6D HOW GOOD AT CREDIT CARD PAYOFF"      
      TA130053      "C1A HOW GOOD AT SUPERVISING COMP"        
      TA130054      "C1B HOW GOOD AT LEADING COMP W/OTRS"     
      TA130055      "C1C HOW GOOD AT LOGIC COMP W/OTRS"       
      TA130056      "C1D HOW GOOD AT HELPING COMP W/OTRS"     
      TA130057      "C1E HOW INTELLIGENT COMPARED W/OTRS"     
      TA130058      "C1F HOW INDEPENDENT COMPARED W/OTRS"     
      TA130059      "C1G HOW CONFIDENT COMPARED W/OTHERS"     
      TA130060      "C1H HOW DECISIVE COMPARED W/OTHERS"      
      TA130061      "C1J HOW WELL LISTEN COMPARED W/OTHERS"   
      TA130062      "C1K HOW GOOD AT TEACHING COMPARED W/OTRS"
      TA130078      "D1 CURRENT MARITAL STATUS"               
      TA130087      "D8 WTR ROMANTIC RELATIONSHIP NOW"        
      TA130100      "D28A NUMBER OF CHILDREN"                 
      TA130136      "E1 EMPLOYMENT STATUS 1ST MENTION"        
      TA130140      "E3A WTR WORKD SINCE JAN 1 OF PRIOR YEAR" 
      TA130347      "E62 WTR EVER WORKED"                     
      TA130482      "E70 WHY NOT LOOKNG FOR WRK IN LAST 4 WKS"
      TA130691      "G1 WTR GRADUATED HIGH SCHOOL"            
      TA130718      "G10 WTR EVER ATTENDED COLLEGE"           
      TA130719      "G11 WTR IN COLLEGE NOW"                  
      TA130763      "G20 WTR CURRENTLY IN VO/TECH TRAINING"   
      TA130813      "H3 HOW MUCH CONDITION LIMITS AMT OF WORK"
      TA130848      "H12 WTR EVER HAD EMOTIONAL/PSYCH PROBS"  
      TA130850      "H12B WTR DEPRESSION"                     
      TA130851      "H12B WTR BIPOLAR DISORDER"               
      TA130852      "H12B WTR SCHIZOPHRENIA"                  
      TA130853      "H12B WTR ANXIETY"                        
      TA130854      "H12B WTR PHOBIAS"                        
      TA130855      "H12B WTR ALCOHOL PROBLEMS"               
      TA130856      "H12B WTR OTHER DRUG PROBLEMS"            
      TA130857      "H12B WTR OBSESSIVE COMPULSIVE DISORDER"  
      TA130866      "H13B HOW MUCH LIMITS NORMAL ACTIVITIES"  
      TA130877      "H15 WTR>2 WKS DEPRESSED IN PAST 12 MOS"  
      TA130878      "H16 WTR>2 WKS NO INTEREST IN LIFE"       
      TA130933      "H27 # OF HOURS OF SLEEP IN 24-HR PERIOD" 
      TA130934      "H28A FREQ OF SNACK INSTEAD OF REGLR MEAL"
      TA130935      "H28B FREQUENCY OF BINGE EATING"          
      TA130938      "H30 # CIGARETTES PER DAY"                
      TA130946      "H37 HOW OFTEN HAVE DRINKS-HD"            
      TA130947      "H38 # ALCOHOLIC DRINKS PER DAY-HD"       
      TA130948      "H39 # DAYS HAD 4-5 DRINKS-HEAD"          
      TA130955      "H44A # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA130956      "H45A # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA130963      "H44B # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA130964      "H45B # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA130971      "H44C # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA130972      "H45C # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA130976      "H42D_B # OF OCCASIONS IN PAST 12 MOS"    
      TA130977      "H42D_C # OF OCCASIONS IN PAST 30 DAYS"   
      TA130984      "H44E # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA130985      "H45E # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA130992      "H44F # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA130993      "H45F # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA131000      "H44G # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA131001      "H45G # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA131091      "L6 HISPANICITY"                          
      TA131092      "L7 RACE MENTION #1"                      
      TA131097      "M1 FREQUENCY OF HAPPINESS IN LAST MONTH" 
      TA131098      "M2 FREQ OF INTEREST IN LIFE IN LAST MO"  
      TA131099      "M3 FREQ OF FEELING SATISFIED IN LAST MO" 
      TA131100      "M4 FREQ OF FEELING CONTRIB TO SOCIETY"   
      TA131101      "M5 FREQ OF FEELING BELONGING TO COMMUNTY"
      TA131102      "M6 FREQ OF FEELING SOCIETY GETTNG BETTER"
      TA131103      "M7 FREQ OF FEELING PEOPLE BASICALLY GOOD"
      TA131104      "M8 FREQ FEELNG WAY SOC WORKS MAKES SENSE"
      TA131105      "M9 FREQ FEEL MANAGNG DAILY RESPONSIBILTY"
      TA131106      "M10 FREQ FEELING TRUSTING RELS W/OTHERS" 
      TA131107      "M11 FREQ OF FEELING CHALLENGED TO GROW"  
      TA131108      "M12 FREQ FEELING CONFIDENT OF OWN IDEAS" 
      TA131109      "M13 FREQ OF FEELING LIKED PERSONALITY"   
      TA131110      "M14 FREQ OF FEELING LIFE HAD DIRECTION"  
      TA131211      "MENTAL HEALTH:  WORRY"                   
      TA131212      "MENTAL HEALTH:  SOCIAL ANXIETY"          
      TA131214      "SUBSCALE:  EMOTIONAL WB"                 
      TA131216      "SUBSCALE:  PSYCHOLOGICAL WB"             
      TA131217      "MENTAL HEALTH:  NON-SPEC PSYCH DISTRESS" 
      TA131223      "BODY MASS INDEX"                         
      TA131225      "ENROLLMENT STATUS"                       
      ER34301       "2015 INTERVIEW NUMBER"                   
      ER34302       "SEQUENCE NUMBER                       15"
      ER34303       "RELATION TO HEAD                      15"
      TA150001      "RELEASE NUMBER"                          
      TA150016      "A1 WTR INVOLVED IN ARTS"                 
      TA150017      "A2 TYPE OF ARTS"                         
      TA150018      "A3 HOW OFTEN PARTICIPATED IN ARTS"       
      TA150019      "A4 WTR MEMBER OF SPORTS TEAM"            
      TA150020      "A5 HOW OFTEN PARTICIPATD IN SPORTS"      
      TA150021      "A6 HOW OFTEN WATCHED OR READ NEWS"       
      TA150022      "A7 HOW OFTEN READ FOR PLEASURE"          
      TA150023      "A8 HOW OFTEN WATCH NON-NEWS TV"          
      TA150024      "A10A WTR USED INTERNET FOR EMAIL"        
      TA150025      "A10B WTR USED INTERNET FOR SCHOOL"       
      TA150026      "A10C WTR USED INTERNET TO SHOP"          
      TA150027      "A10D WTR USED INTERNET FOR GAMES"        
      TA150028      "A10E WTR USED INTERNET FOR SOC NETWRKING"
      TA150029      "A11 WTR VOTED IN 2014"                   
      TA150030      "A11B WTR IN SOCIAL ACTION GROUPS"        
      TA150031      "A11C TYPE OF SOCIAL ACTION GROUPS"       
      TA150032      "A11D HOW OFTEN DID SOCIAL ACTION GROUPS" 
      TA150033      "A12 WTR INVOLVED WITH SCHOOL CLUBS"      
      TA150034      "A12B HOW OFTEN DID SCHOOL CLUBS"         
      TA150035      "A13 WTR DID OTR VOLUNTEER WORK"          
      TA150036      "A14 TYPE VOLUNTEER ORG-MENTION 1"        
      TA150037      "A14 TYPE VOLUNTEER ORG-MENTION 2"        
      TA150038      "A14 TYPE VOLUNTEER ORG-MENTION 3"        
      TA150039      "A14 TYPE VOLUNTEER ORG-MENTION 4"        
      TA150040      "A14 TYPE VOLUNTEER ORG-MENTION 5"        
      TA150041      "A14 TYPE VOLUNTEER ORG-MENTION 6"        
      TA150042      "A14B HOW OFTEN VOLUNTEERED"              
      TA150043      "B1 FALL/WINTER PRIMARY RESIDENCE"        
      TA150044      "B2 SUMMER PRIMARY RESIDENCE"             
      TA150045      "B5A HOW MUCH RESONSIBLTY EARNG OWN LIVNG"
      TA150046      "B5B HOW MUCH RESPONSIBLTY PAYNG OWN RENT"
      TA150047      "B5C HOW MUCH RESPONSBLTY FOR OWN BILLS"  
      TA150048      "B5D HOW MUCH RESPONSIBLTY MANAGING MONEY"
      TA150049      "B6A HOW GOOD AT RESPONSIBILITY"          
      TA150050      "B6B HOW GOOD AT PROBLEM SOLVING"         
      TA150051      "B6C HOW GOOD AT MONEY MANAGEMENT"        
      TA150052      "B6D HOW GOOD AT CREDIT CARD PAYOFF"      
      TA150053      "C1A HOW GOOD AT SUPERVISING COMP"        
      TA150054      "C1B HOW GOOD AT LEADING COMP W/OTRS"     
      TA150055      "C1C HOW GOOD AT LOGIC COMP W/OTRS"       
      TA150056      "C1D HOW GOOD AT HELPING COMP W/OTRS"     
      TA150057      "C1E HOW INTELLIGENT COMPARED W/OTRS"     
      TA150058      "C1F HOW INDEPENDENT COMPARED W/OTRS"     
      TA150059      "C1G HOW CONFIDENT COMPARED W/OTHERS"     
      TA150060      "C1H HOW DECISIVE COMPARED W/OTHERS"      
      TA150061      "C1J HOW WELL LISTEN COMPARED W/OTHERS"   
      TA150062      "C1K HOW GOOD AT TEACHING COMPARED W/OTRS"
      TA150070      "D1 CURRENT MARITAL STATUS"               
      TA150079      "D8 WTR ROMANTIC RELATIONSHIP NOW"        
      TA150092      "D28A NUMBER OF CHILDREN"                 
      TA150128      "E1 EMPLOYMENT STATUS 1ST MENTION"        
      TA150132      "E3A WTR WORKD SINCE JAN 1 OF PRIOR YEAR" 
      TA150349      "E62 WTR EVER WORKED"                     
      TA150491      "E70 WHY NOT LOOKNG FOR WRK IN LAST 4 WKS"
      TA150701      "G1 WTR GRADUATED HIGH SCHOOL"            
      TA150730      "G10 WTR EVER ATTENDED COLLEGE"           
      TA150731      "G11 WTR IN COLLEGE NOW"                  
      TA150776      "G20 WTR CURRENTLY IN VO/TECH TRAINING"   
      TA150826      "H3 HOW MUCH CONDITION LIMITS AMT OF WORK"
      TA150865      "H12 WTR EVER HAD EMOTIONAL/PSYCH PROBS"  
      TA150867      "H12B WTR DEPRESSION"                     
      TA150868      "H12B WTR BIPOLAR DISORDER"               
      TA150869      "H12B WTR SCHIZOPHRENIA"                  
      TA150870      "H12B WTR ANXIETY"                        
      TA150871      "H12B WTR PHOBIAS"                        
      TA150872      "H12B WTR ALCOHOL PROBLEMS"               
      TA150873      "H12B WTR OTHER DRUG PROBLEMS"            
      TA150874      "H12B WTR OBSESSIVE COMPULSIVE DISORDER"  
      TA150883      "H13B HOW MUCH LIMITS NORMAL ACTIVITIES"  
      TA150892      "H15 WTR>2 WKS DEPRESSED IN PAST 12 MOS"  
      TA150893      "H16 WTR>2 WKS NO INTEREST IN LIFE"       
      TA150949      "H27 # OF HOURS OF SLEEP IN 24-HR PERIOD" 
      TA150950      "H28A FREQ OF SNACK INSTEAD OF REGLR MEAL"
      TA150951      "H28B FREQUENCY OF BINGE EATING"          
      TA150954      "H30 # CIGARETTES PER DAY"                
      TA150968      "H37 HOW OFTEN HAVE DRINKS-HD"            
      TA150969      "H38 # ALCOHOLIC DRINKS PER DAY-HD"       
      TA150970      "H39 # DAYS HAD 4-5 DRINKS-HEAD"          
      TA150977      "H44A # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA150978      "H45A # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA150985      "H44B # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA150986      "H45B # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA150993      "H44C # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA150994      "H45C # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA150998      "H42D_B # OF OCCASIONS IN PAST 12 MOS"    
      TA150999      "H42D_C # OF OCCASIONS IN PAST 30 DAYS"   
      TA151006      "H44E # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA151007      "H45E # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA151014      "H44F # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA151015      "H45F # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA151022      "H44G # OF TIMES TOOK W/O DOC IN 12 MOS"  
      TA151023      "H45G # OF TIMES TOOK W/O DOC IN 30 DAYS" 
      TA151131      "L6 HISPANICITY"                          
      TA151132      "L7 RACE MENTION #1"                      
      TA151137      "M1 FREQUENCY OF HAPPINESS IN LAST MONTH" 
      TA151138      "M2 FREQ OF INTEREST IN LIFE IN LAST MO"  
      TA151139      "M3 FREQ OF FEELING SATISFIED IN LAST MO" 
      TA151140      "M4 FREQ OF FEELING CONTRIB TO SOCIETY"   
      TA151141      "M5 FREQ OF FEELING BELONGING TO COMMUNTY"
      TA151142      "M6 FREQ OF FEELING SOCIETY GETTNG BETTER"
      TA151143      "M7 FREQ OF FEELING PEOPLE BASICALLY GOOD"
      TA151144      "M8 FREQ FEELNG WAY SOC WORKS MAKES SENSE"
      TA151145      "M9 FREQ FEEL MANAGNG DAILY RESPONSIBILTY"
      TA151146      "M10 FREQ FEELING TRUSTING RELS W/OTHERS" 
      TA151147      "M11 FREQ OF FEELING CHALLENGED TO GROW"  
      TA151148      "M12 FREQ FEELING CONFIDENT OF OWN IDEAS" 
      TA151149      "M13 FREQ OF FEELING LIKED PERSONALITY"   
      TA151150      "M14 FREQ OF FEELING LIFE HAD DIRECTION"  
      TA151271      "MENTAL HEALTH:  WORRY"                   
      TA151272      "MENTAL HEALTH:  SOCIAL ANXIETY"          
      TA151274      "SUBSCALE:  EMOTIONAL WB"                 
      TA151276      "SUBSCALE:  PSYCHOLOGICAL WB"             
      TA151277      "MENTAL HEALTH:  NON-SPEC PSYCH DISTRESS" 
      TA151283      "BODY MASS INDEX"                         
      TA151285      "ENROLLMENT STATUS"                       
      ER34501       "2017 INTERVIEW NUMBER"                   
      ER34502       "SEQUENCE NUMBER                       17"
      ER34503       "RELATION TO REFERENCE PERSON          17"
      TA170001      "RELEASE NUMBER"                          
      TA170016      "A2 WTR INVOLVED IN ARTS"                 
      TA170017      "A3 TYPE OF ARTS"                         
      TA170018      "A4 HOW OFTEN PARTICIPATED IN ARTS"       
      TA170019      "A5 WTR MEMBER OF SPORTS TEAM"            
      TA170020      "A6 HOW OFTEN PARTICIPATD IN SPORTS"      
      TA170021      "A7 WTR INVOLVED WITH SCHOOL CLUBS"       
      TA170022      "A8 HOW OFTEN DID SCHOOL CLUBS"           
      TA170023      "A9CKPT WTR ELIGIBLE TO VOTE IN 2016"     
      TA170024      "A9 WTR VOTED IN 2016"                    
      TA170025      "A10 WTR IN SOCIAL ACTION GROUPS"         
      TA170026      "A11 TYPE OF SOCIAL ACTION GROUPS"        
      TA170027      "A12 HOW OFTEN DID SOCIAL ACTION GROUPS"  
      TA170028      "A13 WTR DID VOLUNTEER WORK"              
      TA170029      "A14 TYPE VOLUNTEER ORG-MENTION 1"        
      TA170030      "A14 TYPE VOLUNTEER ORG-MENTION 2"        
      TA170031      "A14 TYPE VOLUNTEER ORG-MENTION 3"        
      TA170032      "A14 TYPE VOLUNTEER ORG-MENTION 4"        
      TA170033      "A14 TYPE VOLUNTEER ORG-MENTION 5"        
      TA170034      "A14 TYPE VOLUNTEER ORG-MENTION 6"        
      TA170035      "A15 HOW OFTEN VOLUNTEERED"               
      TA170058      "B15 FALL/WINTER PRIMARY RESIDENCE"       
      TA170059      "B16 SUMMER PRIMARY RESIDENCE"            
      TA170061      "B17A HOW MUCH RESPONSIBLTY EARN OWN LIV" 
      TA170062      "B17B HOW MUCH RESPONSIBLTY PAY OWN RENT" 
      TA170063      "B17C HOW MUCH RESPONSBLTY FOR OWN BILLS" 
      TA170064      "B17D HOW MUCH RESPONSIBLTY MANAGE MONEY" 
      TA170065      "B18A FREQ OF HAPPINESS IN LAST MONTH"    
      TA170066      "B18B FREQ INTEREST IN LIFE IN LAST MO"   
      TA170067      "B18C FREQ FEELING SATISFIED IN LAST MO"  
      TA170068      "B18D FREQ OF FEELING CONTRIB TO SOCIETY" 
      TA170069      "B18E FREQ OF FEELING BELONG TO COMMUNTY" 
      TA170070      "B18F FREQ FEELING SOCIETY GETTNG BETTER" 
      TA170071      "B18G FREQ FEELING PEOPLE BASICALLY GOOD" 
      TA170072      "B18H FREQ FEEL WAY SOC WORK MAKES SENSE" 
      TA170073      "B18J FREQ FEEL MANAGE DAILY RESONSIBLTY" 
      TA170074      "B18K FREQ FEEL TRUSTING RELS W/OTHERS"   
      TA170075      "B18L FREQ OF FEELING CHALLENGED TO GROW" 
      TA170076      "B18M FREQ FEELING CONFIDENT OWN IDEAS"   
      TA170077      "B18N FREQ OF FEELING LIKED PERSONALITY"  
      TA170078      "B18O FREQ OF FEELING LIFE HAD DIRECTION" 
      TA170093      "C1 CURRENT MARITAL STATUS"               
      TA170116      "C12 WTR ROMANTIC RELATIONSHIP NOW"       
      TA170176      "C45 NUMBER OF CHILDREN"                  
      TA170183      "D1 EMPLOYMENT STATUS 1ST MENTION"        
      TA170187      "D3 WTR WORKED SINCE JAN 1 OF PRIOR YEAR" 
      TA170386      "D83 WTR EVER WORKED"                     
      TA170416      "D86 WTR ATTENDED JOB TRAINING PROGRAM"   
      TA170781      "G6 WTR GRADUATED HIGH SCHOOL"            
      TA170790      "G15 WTR ATTENDING OR HAS ATTENDED COLL"  
      TA170795      "G18CKPT WTR ATTEND COLLEGE"              
      TA170866      "H6 HOW MUCH CONDITION LIMITS AMT WORK"   
      TA170905      "H12 WTR EVER HAD EMOTIONAL/PSYCH PROBS"  
      TA170907      "H12B WTR DEPRESSION"                     
      TA170908      "H12B WTR BIPOLAR DISORDER"               
      TA170909      "H12B WTR SCHIZOPHRENIA"                  
      TA170910      "H12B WTR ANXIETY"                        
      TA170911      "H12B WTR PHOBIAS"                        
      TA170912      "H12B WTR ALCOHOL PROBLEMS"               
      TA170913      "H12B WTR OTHER DRUG PROBLEMS"            
      TA170914      "H12B WTR OBSESSIVE COMPULSIVE DISORDER"  
      TA170923      "H13B HOW MUCH LIMITS NORMAL ACTIVITIES"  
      TA171798      "H82 # OF HOURS OF SLEEP IN 24-HR PERIOD" 
      TA171806      "H84 WTR>2 WKS DEPRESSED IN PAST 12 MOS"  
      TA171807      "H85 WTR>2 WKS NO INTEREST IN LIFE"       
      TA171809      "H87 # CIGARETTES PER DAY"                
      TA171825      "H104 HOW OFTEN HAVE DRINKS"              
      TA171826      "H105 # ALCOHOLIC DRINKS PER DAY"         
      TA171827      "H106 # DAYS HAD 4-5 DRINKS"              
      TA171834      "H112 # OF TIMES TOOK W/O DOC IN 12 MO"   
      TA171835      "H113 # OF TIMES TOOK W/O DOC 30 DAYS"    
      TA171839      "H110B # OF OCCASIONS IN PAST 12 MOS"     
      TA171840      "H110C # OF OCCASIONS IN PAST 30 DAYS"    
      TA171852      "H112 # OF TIMES TOOK W/O DOC IN 12 MO"   
      TA171853      "H113 # OF TIMES TOOK W/O DOC 30 DAYS"    
      TA171860      "H112 # OF TIMES TOOK W/O DOC IN 12 MO"   
      TA171861      "H113 # OF TIMES TOOK W/O DOC 30 DAYS"    
      TA171868      "H112 # OF TIMES TOOK W/O DOC IN 12 MO"   
      TA171869      "H113 # OF TIMES TOOK W/O DOC 30 DAYS"    
      TA171876      "H112 # OF TIMES TOOK W/O DOC IN 12 MO"   
      TA171877      "H113 # OF TIMES TOOK W/O DOC 30 DAYS"    
      TA171884      "H112 # OF TIMES TOOK W/O DOC IN 12 MO"   
      TA171885      "H113 # OF TIMES TOOK W/O DOC 30 DAYS"    
      TA171955      "L7 RACE MENTION #1"                      
      TA171960      "L8 HISPANICITY MENTION 1"                
      TA171971      "MENTAL HEALTH:  SOCIAL ANXIETY"          
      TA171972      "SUBSCALE:  EMOTIONAL WB"                 
      TA171974      "SUBSCALE:  PSYCHOLOGICAL WB"             
      TA171975      "MENTAL HEALTH:  NON-SPEC PSYCH DISTRESS" 
      TA171978      "BODY MASS INDEX"                         
      TA171980      "ENROLLMENT STATUS"                       
.
EXECUTE .
