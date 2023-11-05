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
    ;+  ?~  cur-gid.hut-component  ;main: Select a Squad
      ;main
        ;+  %^  huts-component  our.bol  cur-gid.hut-component
          [cur-hut.hut-component ~(tap in (~(get ju huts) cur-gid.hut-component))]
        ;+  ?~  cur-hut.hut-component  ;div.content;
          %-  messages-component  (~(get ja msg-jar) cur-hut.hut-component)
        ;+  %^  people-component  our.bol  cur-gid.hut-component
          [cur-hut.hut-component ~(tap in (~(get ju joined) cur-gid.hut-component))]
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
  |=  [our=@p sel-gid=gid =cur-hut hut-list=(list @tas)]
  ^-  manx
  ;div.huts-menu
    ;h2.section-heading: Huts
    ;+  ?.  =(our host.sel-gid)  ;div;
      ;div.hut-form
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
        ?:  &(?!(=(~ cur-hut)) =(+.cur-hut hut-name))
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
++  messages-component
  |=  =msgs
  ;div.content
    ;div.msgs
      ;*  %+  turn  msgs
        |=  =msg
        ;div.msg
          ;+  ;/  <who.msg>
          ;+  ;/  (trip what.msg)
        ==
    ==
    ;div.chat
      ;textarea#chat-input;
      ;button
        =data-event  "/click/send-message"
        =data-return  "/chat-input/value"
        ;+  ;/  "Send"
      ==
    ==
  ==
::
++  people-component
  |=  [our=@p sel-gid=gid =cur-hut gid-members=(list @p)]
  ;div.people
    ;+  ?:  =(our host.sel-gid)
        ?~  cur-hut  ;div;
        ;button
          =class  "red-button"
          =data-event  "/click/delete-hut"
          ;+  ;/  "Delete Hut"
        ==
      ;button
        =class  "red-button"
        =data-event  "/click/leave-squad"
        ;+  ;/  "Leave Squad"
      ==
    ;h3.section-heading: Members
    ;div.member-list
      ;*  %+  turn  gid-members
        |=  patp=@p
        ;div: {<patp>}
    ==
  ==
::
++  make-gid-str
  |=  =gid
  ^-  tape
  "{=>(<host.gid> ?>(?=(^ .) t))}_{(trip name.gid)}"
--