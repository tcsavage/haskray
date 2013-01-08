/*
Functionality for setting up the UI.
*/

function addStylesheetRules (decls) {
	var style = document.createElement('style');
	document.getElementsByTagName('head')[0].appendChild(style);
	if (!window.createPopup) { /* For Safari */
		style.appendChild(document.createTextNode(''));
	}
	var s = document.styleSheets[document.styleSheets.length - 1];
	for (var i=0, dl = decls.length; i < dl; i++) {
		var j = 1, decl = decls[i], selector = decl[0], rulesStr = '';
		if (Object.prototype.toString.call(decl[1][0]) === '[object Array]') {
			decl = decl[1];
			j = 0;
		}
		for (var rl=decl.length; j < rl; j++) {
			var rule = decl[j];
			rulesStr += rule[0] + ':' + rule[1] + (rule[2] ? ' !important' : '') + ';\n';
		}
		if (s.insertRule) {
			s.insertRule(selector + '{' + rulesStr + '}', s.cssRules.length);
		}
		else { /* IE */
			s.addRule(selector, rulesStr, -1);
		}
	}
}

function buildIconClasses (images) {
	var sheet = document.getElementById("sheet");
	// var images = ["3D-plane", "3D-sphere", "MD-photo", "MD-reload"];
	for(i = 0; i < images.length; i++) {
		var image = images[i];
		var img = "url(icons/" + image + ".png)";
		var rule = img + " no-repeat border"
		addStylesheetRules([
			[".i"+image, ["-webkit-mask", rule], ["-o-mask", rule], ["-moz-mask", rule], ["mask", rule]]
		]);
	}
}

function moveToolTip () {
	var tt = document.getElementById("tooltip");
	var x = window.event.clientX + 5;
	var y = window.event.clientY + 10;
	tt.style.top = y+"px";
	tt.style.left = x+"px";
}

function setupTooltips () {
	//var tt = document.getElementById("tooltip");
	var tt = document.createElement("span");
	tt.setAttribute("id", "tooltip");
	tt.appendChild(document.createTextNode());
	document.getElementsByTagName("body")[0].appendChild(tt);
	tipable = document.getElementsByClassName("tooltip");
	for(i = 0; i < tipable.length; i++) {
		tipable[i].onmouseover = function () {
			this.onmousemove = moveToolTip
			tt.innerHTML = this.getAttribute("data-tooltip");
			detail = this.getAttribute("data-tooltip-detail");
			if (detail) {
				tt.innerHTML += "<small>"+detail+"</small>";
			}
			tt.style.display = "inline";
		};
		tipable[i].onmouseout = function () {
			tt.style.display = "none";
			this.onmousemove = null;
		};
	}
}

function initUi () {
	buildIconClasses(["3D-plane", "3D-sphere", "MD-photo", "MD-reload", "save"]);
	setupTooltips();
}
