import {Greeter} from './sample';
import './style/style.css';
import Icon from './image/icon.png';

function component() {
  let element = document.createElement('div');

  // Lodash, currently included via a script, is required for this line to work
  element.innerHTML = _.join(['Hello', 'webpack'], ' ');
  element.classList.add('hello');

  const g = new Greeter("tom");
  console.info(g.greet());

  // Add the image to our existing div.
  console.info(Icon)
  var myIcon = new Image();
  myIcon.src = Icon;
  element.appendChild(myIcon);

  return element;
}

document.body.appendChild(component());
