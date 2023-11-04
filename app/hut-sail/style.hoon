^~
'''
body {
  font-family: "Inter", sans-serif;
  width: 100vw;
  height: 100vh;
  margin: 0;
}
main {
  height: 100%;
  width: 100%;
  overflow: scroll;
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
}
button, select {
  border: none;
  border-radius: 2rem; 
  font-weight: 500;
  font-size: 1rem;
  padding: 0.7rem;
  margin-block: 0;
  margin-inline: 0.5rem;
  cursor: pointer;
}
button:hover, select:hover, .hut-selector:hover {
  opacity: 0.8;
}
button.active, select.active, .hut-selector:active {
  filter: brightness(1.2);
}
input {
  border: 1px solid #ccc;
  border-radius: 6px;
  padding: 12px;
  font-size: 12px;
  font-weight: 600;
  min-width: 13rem;
}
.top-bar {
  padding: 1rem;
  margin: 1rem;
  border: 1px solid #ccc;
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: center;
}
.gid-title {
  font-weight: 600;
  font-size: 1.3rem;
  margin-block: 0;
  margin-right: 40vw;
  color: #626160;
}
.huts-menu {
  padding-inline: 1rem;
  padding-block: 2rem;
  margin-left: 1rem;
  border: 1px solid #ccc;
  display: flex;
  flex-direction: column;
  justify-content: flex-start;
  align-items: flex-start;
}
.huts-heading {
  font-weight: 600;
  font-size: 1rem;
  margin-top: 0;
  margin-bottom: 1rem;
  color: #626160;
}
.hut-selector {
  border-radius: 2rem; 
  font-weight: 500;
  font-size: 1.2rem;
  padding: 1rem;
  margin-block: 1rem;
  margin-inline: 0;
  background-color: #F4F3F1;
  box-shadow: rgba(0, 0, 0, 0.15) 0px 3px 6px;
  cursor: pointer;
}
.hut-selector.selected {
  background-color: #4eae75;
  color: white;
  cursor: default;
}
'''