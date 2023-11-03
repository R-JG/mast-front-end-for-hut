^~
'''
body {
  @apply bg-white text-wall-600 h-screen w-screen;
  margin: 0;
}
* {
  @apply font-sans;
  font-size: 1rem;
  box-sizing: border-box;
}
div#app {
  height: 100%;
  width: 100%;
  max-width: 100%;
  padding: 2ch;
  display: flex;
  flex-direction: column;
  justify-content: stretch;
}
.top-bar {
  flex: 0 0 auto;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  margin: 1em 0;
}
.gid-title {
  @apply text-ellipsis whitespace-nowrap block overflow-hidden font-semibold;
  margin: 0 1ch;
}
.join-span {
  display: block;
  white-space: nowrap;
}
.join-select {
  width: 9ch;
  text-overflow: ellipsis;
}
.join-button {
  @apply bg-green-400 text-white rounded-md px-2 font-medium ml-2 inline-block text-center;
  border: none;
  cursor: pointer;
}
main {
  @apply flex flex-1 w-full;
  justify-content: stretch;
  height: calc(100% - 16em);
}
.left-menu {
  @apply overflow-hidden flex flex-col h-full border rounded-lg mr-4 p-2 border-wall-200;
  flex: 0 0 auto;
  justify-content: stretch;
}
.hut-list {
  margin-top: auto;
  width: 100%;
  overflow: hidden;
  display: flex;
  flex-direction: column;
  overflow-y: auto;
  flex: 1 1 auto;
}
.make-hut {
  @apply mb-4 rounded-md p-1;
  width: 13ch;
  flex: 0 0 auto;
  display: block;
}
.content {
  @apply border flex flex-col flex-1 border-wall-200 rounded-lg;
  justify-content: stretch;
  overflow: hidden;
}
.msgs {
  height: 100%;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  flex: 1 1 auto;
}
.msg {
  margin: 0.4em 0.4em 0 16ch;
  text-indent: -16ch;
}
.who {
  @apply font-medium;
  width: 16ch;
  text-align: right;
  padding-right: 1ch;
  display: inline-block;
}
.what {
  overflow-wrap: break-word;
  text-overflow: ellipsis;
}
.right-menu {
  height: 100%;
  width: 16ch;
  flex: 0 0 auto;
  padding-left: 2ch;
  display: flex;
  flex-direction: column;
  justify-content: stretch;
}
.leave-button {
  flex: 0 0 auto;
  width: 100%;
  margin-bottom: 1em;
  @apply bg-red rounded-md text-white font-medium cursor-pointer border-0 p-0;
  width: 14ch;
}
.ppl {
  @apply flex-1 border rounded-lg p-2 border-wall-200;
  overflow-y: auto;
}
input, textarea {
  @apply bg-white border border-wall-200;
}
.our {
  width: 14ch;
  display: inline-block;
  text-align: right;
  flex: 0 0 auto;
  align-self: center;
  margin-right: 2ch;
}
.input {
  flex: 0 0 auto;
  display: flex;
  flex-direction: row;
  justify-content: stretch;
  height: 6em;
  margin: 1em 0;
}
textarea {
  flex: 1 1 auto;
  border-radius: 10px;
  margin-right: 1em;
  resize: none;
}
select {
  @apply bg-white;
  border: 0;
  cursor: pointer;
}
.fix {flex: 1 1 auto}
label {
  display: inline-block;
  width: 8ch;
}
.conn {
  flex: 0 0 auto;
  font-size: 0.9em;
  text-align: right;
}
.out {color: rgba(216, 216, 216, 0.5)}
.current-hut {
  @apply text-green-400 border-wall-400 border;
  border-radius: 6px;
}
.other-hut {
  @apply text-wall-500;
}
.other-hut:hover {
  opacity: 0.8;
}
.current-hut, .other-hut {
  overflow: hidden;
  text-overflow: ellipsis;
  padding: 10px 0.5ch;
  margin-right: 1ch;
  flex: 0 0 auto;
  cursor: pointer;
}
.current-hut:first-child, .other-hut:first-child {
  margin-top: auto;
}
.current-hut:last-child, .other-hut:last-child {
  margin-bottom: auto;
}
.selectgid {
  margin-top: 0.5em;
  margin-bottom: 0.5em;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
}
.gid-select {
  width: 13ch;
}
@media only screen and (max-width: 800px) {
  .right-menu {display: none}
  .our {display: none}
  textarea {margin-left: 1em}
  .msg {margin-left: 1ch; text-indent: initial}
  .who {width: initial}
  select {max-width: 14ch}
}
'''