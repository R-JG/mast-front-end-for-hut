/-  *hut, *squad
|=  [bol=bowl:gall =hut-component =huts =msg-jar =joined]
|^  ^-  manx
::
;html
  ;head
    ;meta(charset "utf-8");
    ;link(href "https://fonts.googleapis.com/css2?family=Inter:wght@400;600&family=Source+Code+Pro:wght@400;600&display=swap", rel "stylesheet");
    ;link(href "/hut/style", rel "stylesheet");
  ==
  ;body
    ;+  %+  select-gid-component 
      ~(tap in ~(key by huts)) 
      cur-gid.hut-component
    ;main
      ;+  ?~  cur-gid.hut-component  
          ;div.huts-menu: Select a Squad
        %^  huts-component  our.bol  cur-gid.hut-component
          [cur-hut.hut-component ~(tap in (~(get ju huts) cur-gid.hut-component))]
    ==
  ==
==
::
++  select-gid-component
  |=  [gids=(list gid) =cur-gid]
  ^-  manx
  =/  cur-gid-str=tape  ?~(cur-gid "" (make-gid-str cur-gid))
  ;div.top-bar
    ;select
      =data-event  "/change/select-gid"
      =data-return  "/target/value"
      ;option(value "def"): Squads
      ;*  %+  turn  gids
        |=  =gid
        =/  gid-str=tape  (make-gid-str gid)
        ;option(value gid-str): {gid-str}
    ==
    ;h1.gid-title: {cur-gid-str}
  ==
::
++  huts-component
  |=  [our=@p sel-gid=gid [=cur-hut hut-list=(list @tas)]]
  ^-  manx
  ;div.huts-menu
    ;h2.huts-heading: Chats
    ;+  ?.  =(-.sel-gid our)  ;div;
      ;div
        ;input#new-hut-input;
        ;button
          =data-event  "/click/create-hut"
          =data-return  "/new-hut-input/value"
          =placeholder  "New Hut"
          ;+  ;/  "Create Hut"
        ==
      ==
    ;div
      ;*  %+  turn  hut-list
        |=  hut-name=@tas
        ^-  manx
        ?~  hut-name  ;div;
        ?:  =(+.cur-hut hut-name)  
          ;div(class "hut-selector selected"): {(trip hut-name)}
        ;div
          =class  "hut-selector"
          =data-event  "/click/select-hut"
          =data-return  "/target/textContent"
          ;+  ;/  (trip hut-name)
        ==
    ==
  ==
::
++  make-gid-str
  |=  =gid
  ^-  tape
  "{=>(<host.gid> ?>(?=(^ .) t))}_{(trip name.gid)}"
--