/-  *hut, *squad
/+  default-agent, dbug, agentio, mast
/=  index  /app/hut-sail/index
/=  style  /app/hut-sail/style
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 =display =cur-url =hut-component =huts =msg-jar =joined]
+$  card  card:agent:gall
--
::
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
=<
|_  bol=bowl:gall
+*  this  .
    def   ~(. (default-agent this %.n) bol)
    io    ~(. agentio bol)
    hc    ~(. +> bol)
    routes  %-  limo
      :~  [/hut index]
      ==
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  (~(arvo pass:io /bind) %e %connect `/'hut' %hut)
      (~(watch-our pass:io /squad) %squad /local/all)
  ==
++  on-save  !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old-vase))]
::
++  on-poke
  |=  [=mark =vase]
  |^  ^-  (quip card _this)
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
      %handle-http-request
        (handle-http !<([@ta inbound-request:eyre] vase))
      %json
        ?>  =(our.bol src.bol)
        (handle-client-poke !<(json vase))
      %hut-do
        ?:  =(our.bol src.bol)
          (local !<(hut-act vase))
        (remote !<(hut-act vase))
    ==
  [cards this]
  ::
  ++  handle-http
    |=  [eyre-id=@ta req=inbound-request:eyre]
    ^-  (quip card _state)
    ?.  authenticated.req
      [(make-auth-redirect:mast eyre-id) state]
    ?+  method.request.req  [(make-400:mast eyre-id) state]
      %'GET'
        =/  url=path  (stab url.request.req)
        ?:  =(/hut/style url)
          [(make-css-response:mast eyre-id style) state]
        =/  new-display  (rig:mast routes url [bol hut-component huts msg-jar joined])
        :-  (plank:mast "hut" /display-updates our.bol eyre-id new-display)
        state(display new-display, cur-url url)
    ==
  ::
  ++  handle-client-poke
    |=  json-req=json
    ^-  (quip card _state)
    =/  client-poke  (parse-json:mast json-req)
    ?~  tags.client-poke  !!
    ?~  t.tags.client-poke  !!
    ?+  [i.tags.client-poke i.t.tags.client-poke]  !!
      [%change %select-gid]
        =/  selected-option=@t  (~(got by data.client-poke) '/target/value')
        ?:  =('def' selected-option)  !!
        =/  u-gid=(unit gid)
          %+  rush  selected-option
          ;~(plug fed:ag ;~(pfix cab sym))
        ?~  u-gid  !!
        =/  new-component-state  hut-component(cur-gid u.u-gid, cur-hut ~)
        =/  new-display=manx  
          (rig:mast routes cur-url [bol new-component-state huts msg-jar joined])
        :-  [(gust:mast /display-updates display new-display) ~]
        state(display new-display, hut-component new-component-state)
      [%click %select-hut]
        ?~  cur-gid.hut-component  !!
        =/  selected-hut=hut
          :-  cur-gid.hut-component
          ^-  @tas  (~(got by data.client-poke) '/target/textContent')
        =/  new-component-state  hut-component(cur-hut selected-hut)
        =/  new-display=manx  
          (rig:mast routes cur-url [bol new-component-state huts msg-jar joined])
        :-  [(gust:mast /display-updates display new-display) ~]
        state(display new-display, hut-component new-component-state)
      [%click %create-hut]
        ?~  cur-gid.hut-component  !!
        =/  new-hut=hut
          :-  cur-gid.hut-component
          ^-  @tas  (~(got by data.client-poke) '/new-hut-input/value')
        %-  local-new  new-hut
      [%click %send-message]
        ?~  cur-gid.hut-component  !!
        ?~  cur-hut.hut-component  !!
        ?>  =(cur-gid.hut-component gid.cur-hut.hut-component)
        =/  msg-txt=@t  (~(got by data.client-poke) '/chat-input/value')
        %+  local-post  [our.bol msg-txt]  cur-hut.hut-component
      [%click %delete-hut]
        ?~  cur-hut.hut-component  !!
        %-  local-del  cur-hut.hut-component
      [%click %leave-squad]
        ?~  cur-gid.hut-component  !!
        %-  local-quit  cur-gid.hut-component
    ==
  ::
  ++  local
    |=  act=hut-act
    ^-  (quip card _state)
    ?-    -.act
      %post  (local-post msg.act hut.act)
      %join  (local-join gid.act)
      %quit  (local-quit gid.act)
      %new   (local-new hut.act)
      %del   (local-del hut.act)
    ==
  ::
  ++  local-post
    |=  [=msg =hut]
    ^-  (quip card _state)
    =/  =path
      /(scot %p host.gid.hut)/[name.gid.hut]
    ?.  =(our.bol host.gid.hut)
      :_  state
      :~  (~(poke pass:io path) [host.gid.hut %hut] [mark vase])
      ==
    =/  =msgs  (~(get ja msg-jar) hut)
    =.  msgs
      ?.  (lte 50 (lent msgs))
            [msg msgs]
        [msg (snip msgs)]
    =:  msg-jar  (~(put by msg-jar) hut msgs)
        input-reset-switch.hut-component  !input-reset-switch.hut-component
        ==
    =/  new-display=manx
      (rig:mast routes cur-url [bol hut-component huts msg-jar joined])
    :_  state(display new-display)
    :-  (fact:io hut-did+vase path /all ~)
    [(gust:mast /display-updates display new-display) ~]
  ++  local-join
    |=  =gid
    ^-  (quip card _state)
    ?<  =(our.bol host.gid)
    =/  =path
      /(scot %p host.gid)/[name.gid]
    :_  state
    :~  (~(watch pass:io path) [host.gid %hut] path)
    ==
  ++  local-quit
    |=  =gid
    ^-  (quip card _state)
    =/  =path
      /(scot %p host.gid)/[name.gid]
    =/  to-rm=(list hut)
      %+  turn  ~(tap in (~(get ju huts) gid))
      |=(=name `hut`[gid name])
    =:  msg-jar
          |-
          ?~  to-rm  msg-jar
          $(to-rm t.to-rm, msg-jar (~(del by msg-jar) i.to-rm))
        huts  (~(del by huts) gid)
        joined  (~(del by joined) gid)
        cur-gid.hut-component  ~
        cur-hut.hut-component  ~
        ==
    =/  new-display=manx
      (rig:mast routes cur-url [bol hut-component huts msg-jar joined])
    :_  state(display new-display)
    :+  (gust:mast /display-updates display new-display)
      (fact:io hut-did+vase /all ~)
    ?.(=(our.bol host.gid) [[(~(leave-path pass:io path) [host.gid %hut] path)] ~] ~)
  ++  local-new
    |=  =hut
    ^-  (quip card _state)
    ?>  =(our.bol host.gid.hut)
    ?>  (has-squad:hc gid.hut)
    ?<  (~(has ju huts) gid.hut name.hut)
    =/  =path
      /(scot %p host.gid.hut)/[name.gid.hut]
    =:  cur-hut.hut-component  hut
        input-reset-switch.hut-component  !input-reset-switch.hut-component
        huts     (~(put ju huts) gid.hut name.hut)
        msg-jar  (~(put by msg-jar) hut *msgs)
        joined   (~(put ju joined) gid.hut our.bol)
        ==
    =/  new-display=manx
      (rig:mast routes cur-url [bol hut-component huts msg-jar joined])
    :_  state(display new-display)
    :~  (gust:mast /display-updates display new-display)
        (fact:io hut-did+vase path /all ~)
    ==
  ++  local-del
    |=  =hut
    ^-  (quip card _state)
    ?>  =(our.bol host.gid.hut)
    =/  =path
      /(scot %p host.gid.hut)/[name.gid.hut]
    =:  huts  (~(del ju huts) gid.hut name.hut)
        msg-jar  (~(del by msg-jar) hut)
        cur-hut.hut-component  ~
        ==
    =/  new-display=manx
      (rig:mast routes cur-url [bol hut-component huts msg-jar joined])
    :_  state(display new-display)
    :~  (gust:mast /display-updates display new-display)
        (fact:io hut-did+vase path /all ~)
    ==
  ::
  ++  remote
    |=  act=hut-act
    ?>  ?=(%post -.act)
    ^-  (quip card _state)
    ?>  =(our.bol host.gid.hut.act)
    ?>  (~(has by huts) gid.hut.act)
    ?>  =(src.bol who.msg.act)
    ?>  (~(has ju joined) gid.hut.act src.bol)
    =/  =path  /(scot %p host.gid.hut.act)/[name.gid.hut.act]
    =/  =msgs  (~(get ja msg-jar) hut.act)
    =.  msgs
      ?.  (lte 50 (lent msgs))
        [msg.act msgs]
      [msg.act (snip msgs)]
    ::
    :: here state has been updated with a new message -> update display
    ::
    :_  state(msg-jar (~(put by msg-jar) hut.act msgs))
    :~  (fact:io hut-did+vase path /all ~)
    ==
  --
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?:  ?=([%squad ~] wire)
    ?+    -.sign  (on-agent:def wire sign)
        %kick
      :_  this
      :~  (~(watch-our pass:io /squad) %squad /local/all)
      ==
    ::
        %watch-ack
      ?~  p.sign  `this
      :_  this
      :~  (~(wait pass:io /behn) (add now.bol ~m1))
      ==
    ::
        %fact
      ?>  ?=(%squad-did p.cage.sign)
      =/  =upd  !<(upd q.cage.sign)
      ?+    -.upd  `this
          %init-all
        =/  gid-to-rm=(list gid)
          ~(tap in (~(dif in ~(key by huts)) ~(key by squads.upd)))
        =/  gid-to-add=(list gid)
          ~(tap in (~(dif in ~(key by squads.upd)) ~(key by huts)))
        =.  huts
          |-
          ?~  gid-to-rm  huts
          $(gid-to-rm t.gid-to-rm, huts (~(del by huts) i.gid-to-rm))
        =.  joined
          |-
          ?~  gid-to-rm  joined
          $(gid-to-rm t.gid-to-rm, joined (~(del by joined) i.gid-to-rm))
        =/  hut-to-rm=(list hut)
          %-  zing
          %+  turn  gid-to-rm
          |=  =gid
          (turn ~(tap in (~(get ju huts) gid)) |=(=name `hut`[gid name]))
        =.  msg-jar
          |-
          ?~  hut-to-rm  msg-jar
          $(hut-to-rm t.hut-to-rm, msg-jar (~(del by msg-jar) i.hut-to-rm))
        =^  cards=(list card)  joined
          %+  roll  ~(tap by joined)
          |:  [[gid=*gid ppl=*ppl] cards=*(list card) n-joined=joined]
          =/  =path  /(scot %p host.gid)/[name.gid]
          =/  ppl-list=(list @p)  ~(tap in ppl)
          =;  [n-cards=(list card) n-n-joined=^joined]
            [(weld n-cards cards) n-n-joined]
          %+  roll  ppl-list
          |:  [ship=*@p n-cards=*(list card) n-n-joined=n-joined]
          ?.  ?&  ?|  ?&  pub:(~(got by squads.upd) gid)
                          (~(has ju acls.upd) gid ship)
                      ==
                      ?&  !pub:(~(got by squads.upd) gid)
                          !(~(has ju acls.upd) gid ship)
                      ==
                  ==
                  (~(has ju n-n-joined) gid ship)
              ==
            [n-cards n-n-joined]
          :-  :+  (kick-only:io ship path ~)
                (fact:io hut-did+!>(`hut-upd`[%quit gid ship]) path /all ~)
              n-cards
          (~(del ju n-n-joined) gid ship)
        =/  kick-paths=(list path)
          (turn gid-to-rm |=(=gid `path`/(scot %p host.gid)/[name.gid]))
        =.  cards  ?~(kick-paths cards [(kick:io kick-paths) cards])
        =.  cards
          %+  weld
            %+  turn  gid-to-rm
            |=  =gid
            ^-  card
            (fact:io hut-did+!>(`hut-upd`[%quit gid our.bol]) /all ~)
          cards
        :: adding new squads to state with a null hut to initialize:
        =:  huts    (~(gas ju huts) (turn gid-to-add |=(=gid [gid %$])))
            joined  (~(gas by joined) (turn gid-to-add |=(=gid [gid (~(get ju members.upd) gid)])))
            ==
        [cards this(huts huts, msg-jar msg-jar, joined joined)]
      ::
      :: adding init for new squad updates:
          %init
        =:  huts    (~(put ju huts) gid.upd %$)
            joined  (~(gas ju joined) (turn ~(tap in ppl.upd) |=(patp=@p [gid.upd patp])))
            ==
        =/  new-display=manx
          (rig:mast routes cur-url [bol hut-component huts msg-jar joined])
        :_  this(display new-display)
        [(gust:mast /display-updates display new-display) ~]
      ::
          %del
        =/  =path  /(scot %p host.gid.upd)/[name.gid.upd]
        =/  to-rm=(list hut)
          %+  turn  ~(tap in (~(get ju huts) gid.upd))
          |=(=name `hut`[gid.upd name])
        =:  msg-jar
              |-
              ?~  to-rm  msg-jar
              $(to-rm t.to-rm, msg-jar (~(del by msg-jar) i.to-rm))
            huts     (~(del by huts) gid.upd)
            joined   (~(del by joined) gid.upd)
            cur-gid.hut-component  ?:(=(gid.upd cur-gid.hut-component) ~ cur-gid.hut-component)
            cur-hut.hut-component  
              ?:  &(?!(=(~ cur-hut.hut-component)) =(gid.upd -.cur-hut.hut-component)) 
                ~ 
              cur-hut.hut-component
            ==
        =/  new-display=manx
          (rig:mast routes cur-url [bol hut-component huts msg-jar joined])
        :_  this(display new-display)
        :^    (gust:mast /display-updates display new-display)
            (kick:io path ~)
          (fact:io hut-did+!>(`hut-upd`[%quit gid.upd our.bol]) /all ~)
        ?.(=(our.bol host.gid.upd) [[(~(leave-path pass:io path) [host.gid.upd %tally] path)] ~] ~)
      ::
          %kick
        =/  =path  /(scot %p host.gid.upd)/[name.gid.upd]
        ?.  =(our.bol ship.upd)
          ::
          :: here state is being updated to remove a member -> update display
          ::
          :_  this(joined (~(del ju joined) gid.upd ship.upd))
          :-  (kick-only:io ship.upd path ~)
          ?.  (~(has ju joined) gid.upd ship.upd)
            ~
          ~[(fact:io hut-did+!>(`hut-upd`[%quit gid.upd ship.upd]) path /all ~)]
        =/  hut-to-rm=(list hut)
          (turn ~(tap in (~(get ju huts) gid.upd)) |=(=name `hut`[gid.upd name]))
        =.  msg-jar
          |-
          ?~  hut-to-rm  msg-jar
          $(hut-to-rm t.hut-to-rm, msg-jar (~(del by msg-jar) i.hut-to-rm))
        ::
        :: here state is being updated to remove the hut and messages that we have been kicked from -> update display
        ::
        :_  %=  this
               huts     (~(del by huts) gid.upd)
               msg-jar  msg-jar
               joined   (~(del by joined) gid.upd)
            ==
        :+  (kick:io path ~)
          (fact:io hut-did+!>(`hut-upd`[%quit gid.upd ship.upd]) /all ~)
        ?:  =(our.bol host.gid.upd)
          ~
        ~[(~(leave-path pass:io path) [host.gid.upd %tally] path)]
      ::
          %leave
        =/  =path  /(scot %p host.gid.upd)/[name.gid.upd]
        ?.  =(our.bol ship.upd)
          ?.  (~(has ju joined) gid.upd ship.upd)
            `this
          ::
          :: here state is being updated to remove a member -> update display
          ::
          :_  this(joined (~(del ju joined) gid.upd ship.upd))
          :~  (kick-only:io ship.upd path ~)
              %+  fact:io
                hut-did+!>(`hut-upd`[%quit gid.upd ship.upd])
              ~[path /all]
          ==
        =/  hut-to-rm=(list hut)
          (turn ~(tap in (~(get ju huts) gid.upd)) |=(=name `hut`[gid.upd name]))
        =.  msg-jar
          |-
          ?~  hut-to-rm  msg-jar
          $(hut-to-rm t.hut-to-rm, msg-jar (~(del by msg-jar) i.hut-to-rm))
        ::
        :: here state is being updated to remove the messages and hut from which we left -> update display
        ::
        :_  %=  this
              huts     (~(del by huts) gid.upd)
              msg-jar  msg-jar
              joined   (~(del by joined) gid.upd)
            ==
        :+  (kick:io path ~)
          (fact:io hut-did+!>(`hut-upd`[%quit gid.upd ship.upd]) /all ~)
        ?:  =(our.bol host.gid.upd)
          ~
        ~[(~(leave-path pass:io path) [host.gid.upd %tally] path)]
      ==
    ==
  ?>  ?=([@ @ ~] wire)
  =/  =gid  [(slav %p i.wire) i.t.wire]
  ?+    -.sign  (on-agent:def wire sign)
      %watch-ack
    ?~  p.sign  `this
    =/  to-rm=(list hut)
      %+  turn  ~(tap in (~(get ju huts) gid))
      |=(=name `hut`[gid name])
    =.  msg-jar
      |-
      ?~  to-rm  msg-jar
      $(to-rm t.to-rm, msg-jar (~(del by msg-jar) i.to-rm))
    ::
    :: here state is being updated to remove a hut and its messages -> update display
    ::
    :-  :~  (fact:io hut-did+!>(`hut-upd`[%quit gid our.bol]) /all ~)
        ==
    %=  this
      huts     (~(del by huts) gid)
      msg-jar  msg-jar
      joined   (~(del by joined) gid)
    ==
  ::
      %kick
    :_  this
    :~  (~(watch pass:io wire) [host.gid %hut] wire)
    ==
  ::
      %fact
    ?>  ?=(%hut-did p.cage.sign)
    =/  upd  !<(hut-upd q.cage.sign)
    ?+    -.upd  (on-agent:def wire sign)
        %init
      ?.  =([gid ~] ~(tap in ~(key by huts.upd)))
        `this
      ?.  =([gid ~] ~(tap in ~(key by joined.upd)))
        `this
      =.  msg-jar.upd
        =/  to-rm=(list [=hut =msgs])
          %+  skip  ~(tap by msg-jar.upd)
          |=  [=hut =msgs]
          ?&  =(gid gid.hut)
              (~(has ju huts.upd) gid.hut name.hut)
          ==
        |-
        ?~  to-rm
          msg-jar.upd
        $(to-rm t.to-rm, msg-jar.upd (~(del by msg-jar.upd) hut.i.to-rm))
      ::
      :: here state is being updated to sync state with a subscription hut init, and cards are produced for the current front end -> update display and change these cards
      ::
      :-  :~  %+  fact:io
                hut-did+!>(`hut-upd`[%init huts.upd msg-jar.upd joined.upd])
              ~[/all]
          ==
      %=  this
        huts     (~(uni by huts) huts.upd)
        msg-jar  (~(uni by msg-jar) msg-jar.upd)
        joined   (~(uni by joined) joined.upd)
      ==
    ::
        %post
      ?.  =(gid gid.hut.upd)
        `this
      =/  msgs  (~(get ja msg-jar) hut.upd)
      =.  msgs
        ?.  (lte 50 (lent msgs))
          [msg.upd msgs]
        [msg.upd (snip msgs)]
      ::
      :: here state is being updated with a new message, and cards for the current front end are produced -> update display and change the cards
      ::
      :_  this(msg-jar (~(put by msg-jar) hut.upd msgs))
      :~  (fact:io cage.sign /all ~)
      ==
    ::
        %join
      ?.  =(gid gid.upd)
        `this
      ::
      :: here state is being updated with a new member, and cards for the current front end are produced -> update display and change the cards
      ::
      :_  this(joined (~(put ju joined) gid who.upd))
      :~  (fact:io cage.sign /all ~)
      ==
    ::
        %quit
      ?.  =(gid gid.upd)
        `this
      ::
      :: here state is being updated to remove a member, and cards for the current front end are produced -> update display and change the cards
      ::
      :_  this(joined (~(del ju joined) gid who.upd))
      :~  (fact:io cage.sign /all ~)
      ==
    ::
        %del
      ?.  =(gid gid.hut.upd)
        `this
      ::
      :: here state is being updated to remove a hut, and cards for the current front end are produced -> update display and change the cards
      ::
      :-  :~  (fact:io cage.sign /all ~)
          ==
      %=  this
        huts     (~(del ju huts) hut.upd)
        msg-jar  (~(del by msg-jar) hut.upd)
      ==
    ==
  ==
::
++  on-watch
  |=  =path
  |^  ^-  (quip card _this)
  ?:  &(=(our.bol src.bol) ?=([%http-response *] path))
    `this
  ?:  &(=(our.bol src.bol) ?=([%display-updates *] path))
    `this
  ?:  ?=([%all ~] path)
    ?>  =(our.bol src.bol)
    :_  this
    :~  %-  fact-init:io
        hut-did+!>(`hut-upd`[%init-all huts msg-jar joined])
    ==
  ?>  ?=([@ @ ~] path)
  =/  =gid  [(slav %p i.path) i.t.path]
  ?>  =(our.bol host.gid)
  ?>  (is-allowed:hc gid src.bol)
  ::
  :: here the member list is being updated for someone to join -> set component state for a notification of this, and update display to render this notification
  ::
  :_  this(joined (~(put ju joined) gid src.bol))
  :-  (init gid)
  ?:  (~(has ju joined) gid src.bol)
    ~
  ~[(fact:io hut-did+!>(`hut-upd`[%join gid src.bol]) /all path ~)]
  ::
  ++  init
    |=  =gid
    ^-  card
    =/  hut-list=(list hut)
      %+  turn  ~(tap in (~(get ju huts) gid))
      |=(=name `hut`[gid name])
    %-  fact-init:io
    :-  %hut-did
    !>  ^-  hut-upd
    :^    %init
        (~(put by *^huts) gid (~(get ju huts) gid))
      %-  ~(gas by *^msg-jar)
      %+  turn  hut-list
      |=(=hut `[^hut msgs]`[hut (~(get ja msg-jar) hut)])
    (~(put by *^joined) gid (~(put in (~(get ju joined) gid)) src.bol))
  --
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  ?:  ?=([%display-updates *] path)
    `this
  ?:  ?=([%all ~] path)
    `this
  ?>  ?=([@ @ ~] path)
  =/  =gid  [(slav %p i.path) i.t.path]
  =/  last=?
    %+  gte  1
    (lent (skim ~(val by sup.bol) |=([=@p *] =(src.bol p))))
  ::
  :: here if last is true, a subscriber is leaving -> in this case, update component state with a message for this, and update display to render the message
  ::
  :_  this(joined (~(del ju joined) gid src.bol))
  ?.  last
    ~
  :~  (fact:io hut-did+!>(`hut-upd`[%quit gid src.bol]) /all path ~)
  ==
::
++  on-peek  on-peek:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?.  ?=([%behn ~] wire)
    (on-arvo:def [wire sign-arvo])
  ?>  ?=([%behn %wake *] sign-arvo)
  ?~  error.sign-arvo
    :_  this
    :~  (~(watch-our pass:io /squad) %squad /local/all)
    ==
  :_  this
  :~  (~(wait pass:io /behn) (add now.bol ~m1))
  ==
++  on-fail  on-fail:def
--
::
|_  bol=bowl:gall
++  has-squad
  |=  =gid
  ^-  ?
  =-  ?=(^ .)
  .^  (unit)
    %gx
    (scot %p our.bol)
    %squad
    (scot %da now.bol)
    %squad
    (scot %p host.gid)
    /[name.gid]/noun
  ==
++  is-allowed
  |=  [=gid =ship]
  ^-  ?
  =/  u-acl
    .^  (unit [pub=? acl=ppl])
      %gx
      (scot %p our.bol)
      %squad
      (scot %da now.bol)
      %acl
      (scot %p host.gid)
      /[name.gid]/noun
    ==
  ?~  u-acl  |
  ?:  pub.u.u-acl
    !(~(has in acl.u.u-acl) ship)
  (~(has in acl.u.u-acl) ship)
--
