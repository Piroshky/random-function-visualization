# random-function-visualization

A Common Lisp project for creating interesting images with randomly generated functions.

### Examples:
<p align="center">
<a href="https://i.imgur.com/VMglKHU.png"><img src="https://i.imgur.com/VMglKHU.png" width="300" height="300"></a>
<a href="https://i.imgur.com/l8OYTux.png"><img src="https://i.imgur.com/l8OYTux.png" width="300" height="300"></a>
<a href="https://i.imgur.com/VzfcCdx.png"><img src="https://i.imgur.com/VzfcCdx.png" width="300" height="300"></a>
<a href="https://i.imgur.com/Kguy5JP.png"><img src="https://i.imgur.com/Kguy5JP.png" width="300" height="300"></a>
<a href="https://i.imgur.com/ZvapKnh.png"><img src="https://i.imgur.com/ZvapKnh.png" width="300" height="300"></a>
<a href="https://i.imgur.com/DERByjN.png"><img src="https://i.imgur.com/DERByjN.png" width="300" height="300"></a>
</p>

### Explanation and more examples:
https://piroshky.org/random-function-visualization.html

### GUI:
You can download a binary release for linux [here](https://github.com/Pyrizhki/random-function-visualization/releases/tag/v1.0), or you can load the rfv-gui package
found in the directory of the same name, and run (rfv-gui:main).

<a href="https://i.imgur.com/OpFLyjG.png"><img src="https://i.imgur.com/OpFLyjG.png" height="600"></a>

The GUI will save images and their text forms in the specified directory. The text forms
will all be put into a file called "functions". Saved images can be reloaded with the Open
Image button, this requires that there is a text form with the same name in the functions
file.

The Load Text button will try to construct an image based on what is in the text field. If
the text is simply a function then it will use the size and color settings as set by the
GUI. If the text is a full form, like the type saved in the functions file, it will use
the provided values.
