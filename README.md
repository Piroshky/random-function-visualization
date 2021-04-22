# random-function-visualization

A Common Lisp project that creates interesting pictures through randomly generating functions.

#### Examples:

<img src="https://i.imgur.com/VMglKHU.png" width="500" height="500">

<img src="https://i.imgur.com/l8OYTux.png" width="500" height="500">

<img src="https://i.imgur.com/ZvapKnh.png" width="500" height="500">

<img src="https://i.imgur.com/DERByjN.png" width="500" height="500">

#### GUI:

<img src="https://i.imgur.com/OpFLyjG.png">

You can download a binary release for linux [here](), or you can load the rfv-gui package
found in the directory of the same name, and run (rfv-gui:main).

The GUI will save images and their text forms in the specified directory. The text forms
will all be put into a file called "functions". Saved images can be reloaded with the Open
Image button, this requires that there is a text form with the same name in the functions
file.

The Load Text button will try to construct an image based on what is in the text field. If
the text is simply a function then it will use the size and color settings as set by the
GUI. If the text is a full form, like the type saved in the functions file, it will use
the provided values.


#### How it works:

The functions take as arguments an x and y position, and a color value, and will return a number. Each pixel position is given to the function to produce that pixel's value. The purpose of the "color value" parameter is to add in the possibility of different outputs for different color channels.