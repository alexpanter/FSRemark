function expand(par, arr){
    var len = arr.length;
    if (par.getAttribute("show") === "0") {
	console.log("inside");
	for (var i = 0; i < len; i++) {
	    arr[i].style.display = 'block';
	}
	par.setAttribute("show", "1");
    } else {
	for (var i = 0; i < len; i++) {
	    arr[i].style.display = 'none';
	}
	par.setAttribute("show", "0");
    }
}
function expandSection(obj){
    var par = obj.parentElement;
    var arr = par.getElementsByClassName("bt-question");
    if (par.getAttribute("show") === "1"){
	expand(par, par.getElementsByClassName("mark-container"));
	par.setAttribute("show", "1");
    }
    expand(par, arr);
}
function expandQuestion(obj){
    var par = obj.parentElement;
    var arr = par.getElementsByClassName("mark-container");
    expand(par, arr);
}
