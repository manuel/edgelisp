/*
	JWIM - Javascript Windowmanager for Independent Modules
	
	Copyright (c) 2010 Filippo Cavallarin - poplix@papuasia.org
	
	Version: beta-0.2 2010-05-11
	
	Licensed under the MIT license http://www.opensource.org/licenses/mit-license.php
	
	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:
	
	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.
	
	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.

*/



var jwim={
	Utils:{},
	Callbacks:{},
	Scope:function(thisw){
		this.thisw=thisw;
		this._eval=function(s){eval(s);}
	}
};




/* - - MANAGER CLASS - - */

jwim.Manager=function(options){
	
	this.windows=[];
	this.nextid=1;
	this.selectedWindow=null;		
	this.mouse = {cur:{x:0,y:0}, click:{x:0,y:0}};
	this.draggingWin=null;

	this.moveCallback=jwim.Callbacks.move(this);
	this.resizeCallback=jwim.Callbacks.resize(this);
	this.dragStop=jwim.Callbacks.dragStop(this);
	
	this.opts={
		classWindow:'JMWindow',
		classActiveWindow:'activeJMWindow',
		classContent:'content',
		classTitle:'title',
		classIcon:'JMIcon',
		classActiveIcon:'activeJMIcon',
		imageDir:'img/jwim',
		baseZIndex:5000,
		alwaysShowIcons:true,
		taskbar:null,
		container:document.body,
		showTitleBar:true,
		showMaximizer:true,
		showIconizer:true,
		showResizer:true,
		showClose:true,
		defaultX:10,
		defaultY:10,
		defaultWidth:500,
		defaultHeight:350,
		defaultState:'window',
		buttonSize:16,
		shiftOffset:20,
		bottomMargin:16,
		movable:true,
		httpErrorHandler:null,
		autoCheckResize:true,
		containerBoundaries:40,
		iconWidth:120,
		resizeIcons:true,
		afterLoad:null,
		cacheXHR:false
	};

	if(options && typeof options == 'object'){
		for(var a in options)
			this.opts[a]=options[a];
	}
	
	if(this.opts.autoCheckResize)
		jwim.Utils.addEvent(window,'resize',(function(jwm){ return function(){ jwm.checkResize();}})(this));
};		




jwim.Manager.prototype.createWindow=function(opts){
	opts= opts || {};		
	var btoff=3;
	var btcss='position:absolute;top:2px;width:'+this.opts.buttonSize+'px;height:'+this.opts.buttonSize+'px;';		
	var btnDown=function(e){
		this.style.opacity='.5';
		if(!e)
			window.event.cancelBubble = true; // avoid select window
		else
			e.stopPropagation();
		return false;
	}; 
	var btnUp=function(e){ this.style.opacity='1';};

	if(opts.id && this.getWindow(opts.id) != null) return null;
	
	var container= opts.container || this.opts.container;
	var state = opts.state || this.opts.defaultState;
	
	var w = new jwim.Window(opts, this);
	
	w.win =document.createElement('div');		
	w.content=document.createElement('div');
	w.loading=document.createElement('div');

	if(this.opts.shiftOffset > 0) {
		var a;
		do {
			for(a=0; a< this.windows.length; a++){
				var p=this.windows[a];
				if( (p.x==w.x || p.userX==w.x) && (p.y==w.y || p.userY==w.y) ){
					w.x += this.opts.shiftOffset;
					w.y += this.opts.shiftOffset;
					break;
				}
			}
		} while(a != this.windows.length);
	}

	w.userX=w.x;
	w.userY=w.y;
	w.win.className=this.opts.classWindow;
	w.win.style.cssText="z-index:"+this.opts.baseZIndex+";position:absolute;top:"+(w.y)+"px;left:"+(w.x)+"px;";				
	
	//margins and position on content are not allowed
	// IMPORTANT: init width n height
	w.content.className=this.opts.classContent;
	w.content.style.cssText='position:relative;margin:0;width:0px;height:0px';

	
	w.win.onmousedown=jwim.Callbacks.dragStart(w);
	
	
	if(this.opts.showTitleBar){

		w.title=document.createElement('div');
		w.titleSpan=document.createElement('span');
	
		w.title.style.cssText='position:relative;cursor:default';
		w.titleSpan.innerHTML=w.titleText;
		w.title.appendChild(w.titleSpan);
		w.title.onmousedown=function(){return false};
		w.title.className=this.opts.classTitle;	
		
		if(this.opts.showClose){	
			w.components.close=document.createElement('div');	
			w.components.close.style.cssText=btcss+'right:'+btoff+'px;background-image:url('+this.opts.imageDir+'/close.png)';
			w.components.close.onclick=function(){w.close()};
			w.components.close.onmousedown=btnDown;
			w.components.close.onmouseout=btnUp;		
			w.title.appendChild(w.components.close);
			btoff+=this.opts.buttonSize+2;
		}
		
		
		if(this.opts.showMaximizer){
			w.components.maximizer=document.createElement('div');
			w.components.maximizer.style.cssText=btcss+'right:'+btoff+'px;background-image:url('+this.opts.imageDir+'/maximize.png);';			
			w.components.maximizer.onclick=function(){w._maximize()};
			w.components.maximizer.onmousedown=btnDown;
			w.components.maximizer.onmouseout=btnUp;
			w.title.appendChild(w.components.maximizer);
			btoff+=this.opts.buttonSize+2;
		}
		
		
		if(this.opts.taskbar && this.opts.showIconizer){
			w.components.iconizer=document.createElement('div');
			w.components.iconizer.style.cssText=btcss+'right:'+btoff+'px;background-image:url('+this.opts.imageDir+'/iconize.png);';			
			w.components.iconizer.onclick=function(e){w._iconify();};
			w.components.iconizer.onmousedown=btnDown;
			w.components.iconizer.onmouseout=btnUp;
			w.title.appendChild(w.components.iconizer);
			btoff+=this.opts.buttonSize+2;
		}
		
		w.win.appendChild(w.title);			
		w.components.titlebar=w.title;
	} 

	w.win.appendChild(w.content);
	w.components.content=w.content;
	w.loading.style.cssText=btcss+'right:'+btoff+'px;background-image:url('+this.opts.imageDir+'/loading.gif);visibility:hidden;';
	if(w.title)
		w.title.appendChild(w.loading);
	else
		w.win.appendChild(w.loading);
	w.components.loading=w.loading;
	
	if(this.opts.showResizer){
		w.resizer=document.createElement('div');
		w.resizer.style.cssText='position:absolute;background-image:url('+this.opts.imageDir+'/resize.png);width:14px;height:14px;right:1px;bottom:1px';
		w.win.appendChild(w.resizer);
		w.components.resizer=w.resizer;
	}		
		
			
	container.appendChild(w.win);
	w.components.window=w.win;
	w._setSize(w.w,w.h,false,'');
	this.windows.push(w);
	
	if(this.opts.taskbar && this.opts.alwaysShowIcons)w._createTaskbarIcon();
	
	this._selectWindow(w,true);
	if(state != 'window')w.setState(state);
	return w;
	
};


jwim.Manager.prototype.getWindow=function(id){
	for(var a=0; a< this.windows.length; a++){
		if(this.windows[a].id == id) return this.windows[a];
	}
	return null;
};


jwim.Manager.prototype.getSelectedWindow=function(){ 
	return this.selectedWindow;
};


jwim.Manager.prototype.getAllWindows=function(){
	var ret=[];
	for(var a = this.windows.length-1; a >=0  ; a--){
		ret.push(this.windows[a]);
	}
	return ret;
};


jwim.Manager.prototype.getContainerHeight=function(){
	if(this.opts.container == document.body)
		return jwim.Utils.getBodySize().height;
		
	return this.opts.container.clientHeight;

};


jwim.Manager.prototype.getContainerWidth=function(){
	if(this.opts.container == document.body)
		return jwim.Utils.getBodySize().width;

	return this.opts.container.clientWidth;
};


jwim.Manager.prototype.checkResize=function(){
	for(var a=0; a< this.windows.length;a++){
		if(this.windows[a].state != 'maximized')continue;
	
		var Dw= this.windows[a].win.offsetWidth - parseInt(this.windows[a].win.style.width);
		var Dh= this.windows[a].win.offsetHeight - parseInt(this.windows[a].win.style.height);	
		var w=this.windows[a].getContainerWidth()  - Dw;
		var h=this.windows[a].getContainerHeight() - Dh;
		
		this.windows[a]._setSize(w,h,false,'resize');	
	}
	
	if(this.opts.taskbar && this.opts.resizeIcons)
		this._resizeIcons();
}


jwim.Manager.prototype._getNewId=function(){
	var id=this.nextid;
	this.nextid++;
	return id;
};


jwim.Manager.prototype._selectWindow=function(win){
	
	// if win.scope==null the win has been closed and this is the result of event propagation
	if(win == this.selectedWindow || !win.scope)return;
	if(this.selectedWindow && this.selectedWindow.ondeselect)
		this.selectedWindow.ondeselect();
		
	if(!win.alwaysOnTop){
		for(var a=0; a< this.windows.length; a++){
			if(this.windows[a] == win){
				var tmp=this.windows.splice(a,1);
				for(var b=this.windows.length-1; b>=0; b--)
					if(!this.windows[b].alwaysOnTop)break;			
				this.windows.splice(b+1,0,tmp[0]);
				break;
			}
		}
	}
	
	for(var a=0; a< this.windows.length; a++){ // no need to restack all windows...
		this.windows[a].win.style.zIndex=(this.opts.baseZIndex + a);
		this.windows[a].win.className=this.opts.classWindow;
		if(this.windows[a].taskbarIcon)
			this.windows[a].taskbarIcon.className=this.opts.classIcon;
	}
	win.win.style.visibility='inherit';
	if(!this.opts.alwaysShowIcons && win.taskbarIcon != null){
		win._removeTaskbarIcon();
	}
	win.win.className=this.opts.classActiveWindow;
	if(win.taskbarIcon)
		win.taskbarIcon.className=this.opts.classActiveIcon;
	this.selectedWindow=win;
	if(win.state=='icon' || win.state=='hidden'){
		win.state=win.prvState;
		win.prvState=null;
	}
	if(win.onselect)win.onselect();
};


jwim.Manager.prototype._destroyWindow=function(w){
	for(var a=0; a< this.windows.length; a++){
		if(this.windows[a]==w){	
			if( !this.windows[a]._unload(false)) return false;
			//this.opts.container.removeChild(this.windows[a].win);
			this.windows[a].getContainer().removeChild(this.windows[a].win);
			if(this.windows[a].taskbarIcon != null){				
				this.windows[a]._removeTaskbarIcon();
			}
			// save a reference to removed win to handle onclose() event
			var closed=this.windows.splice(a,1)[0];
			for(var b=0; b< this.windows.length; b++){
				this.windows[b].win.style.zIndex=(this.opts.baseZIndex + b);
			}
			if(closed.onclose)
				closed.onclose();
				
			closed.scope=null;
			closed=null;	
			
			var fw=this._getFirstVisibleWindow();
			if(fw)fw.select();
			
			return ;
		}
	}
	return null;

};


jwim.Manager.prototype._getFirstVisibleWindow=function(){
	for(var a = (this.windows.length-1); a>=0 ; a--){
		if(this.windows[a].win.style.visibility != 'hidden')
			return this.windows[a];
	}
	
	return null;
};


jwim.Manager.prototype._resizeIcons=function(){

	function gs(e,p){
		var n = parseInt(jwim.Utils.getStyle(e,p));
		return ( isNaN(n) ? 0 : n );
	}
	
	var a;
	var icns=[];
	//save icons attached to default icons container
	for(a=0; a<this.windows.length;a++){
		if(this.windows[a].taskbarIcon && this.windows[a].taskbarIcon.parentNode == this.opts.taskbar)
			icns.push(this.windows[a].taskbarIcon);
	}
	
	if(icns.length < 1)return;
	
	var tbw = this.opts.taskbar.clientWidth - gs(this.opts.taskbar,'padding-left') - gs(this.opts.taskbar,'padding-right') - 1;
	var iw = parseInt(tbw/icns.length);
	
	for(a=0; a<icns.length;a++){		
		var Dw = ( icns[a].offsetWidth + gs(icns[a],'margin-left') + gs(icns[a],'margin-right') ) - gs(icns[a],'width');		
		if( (this.opts.iconWidth + Dw) > iw){
			icns[a].style.width=(iw-Dw-1)+'px';		
		} else {
			icns[a].style.width = this.opts.iconWidth + 'px';
		}
	}
}	



/* - - WINDOW CLASS - - */

jwim.Window=function(opts,manager){
	this.wm=manager;
	this.manager=manager;
	this.id=opts.id || manager._getNewId();		
	this.w=(typeof opts.width == 'number' ?  opts.width : manager.opts.defaultWidth);
	this.h=(typeof opts.height == 'number' ?  opts.height : manager.opts.defaultHeight);
	this.x=(typeof opts.x == 'number' ?  opts.x : manager.opts.defaultX);
	this.y=(typeof opts.y == 'number' ?  opts.y : manager.opts.defaultY);
	
	this.userW=this.w;
	this.userH=this.h;
	this.userX=null;//this.x; - init by createWindow
	this.userY=null;//this.y;
	this.movable= (typeof opts.movable == 'undefined' ? manager.opts.movable : opts.movable);
	this.alwaysOnTop = opts.alwaysOnTop || false;
	this.titleText= opts.title || '';
	this.title=null;
	this.titleSpan=null;
	this.state= 'window';
	this.prvState=null;
	this.win=null;
	this.content=null;
	this.resizer=null;
	this.taskbarIcon=null;
	this.taskbarIconSpan=null;
	this.currentUrl=null;
	this.afterLoad= opts.afterLoad || manager.opts.afterLoad ;
	
	//initizlized via in-win scripts		
	this.onunload = null;
	this.onselect = null;
	this.onresize=null;
	this.onmove=null;
	this.oniconize=null;
	this.ondeselect=null;
	this.onclose=null;
	this.onhttperror=null;

	this.scope= new jwim.Scope(this);
	
	this.components={
		titlebar:null,
		window:null,
		content:null,
		maximizer:null,
		iconizer:null,
		close:null,
		resizer:null,
		icon:null,
		loading:null
	};
	
};


jwim.Window.prototype.getElement=function(eid,o){
	if(!o) var o = this.content;			
	for(var a=0; a<o.childNodes.length; a++){
		if(o.childNodes[a].getAttribute && o.childNodes[a].getAttribute("data-wid") == eid ){ 
			return o.childNodes[a];
		} 
		var t=this.getElement(eid,o.childNodes[a]);
		if(t)return t;
	}
	return null;
};


jwim.Window.prototype.setState=function(state){
	switch(state){
		case 'maximized':
			if(this.state=='maximized') return;
			this._maximize();
			return;
		case 'window':
			if(this.state=='maximized'){
				this._maximize();
				return;
			}
			this.state='window';
			this.select();
			return;
		case 'icon':
			this._iconify();
			return;
		case 'hidden':
			this.hide();
			return;
	}
}


jwim.Window.prototype.setSize=function(w,h){
	this.state='window';
	this._setSize(w,h,false,'resize');
	this.userW=w;
	this.userH=h;
};


jwim.Window.prototype.setPosition=function(x,y){
	this._setPosition(x,y);
	if(this.onmove) 
		this.onmove(x,y);

};


jwim.Window.prototype._setPosition=function(x,y){ 
	this.state='window';
	this.win.style.left=x+'px';
	this.win.style.top=y+'px'
	this.x = this.userX = x;
	this.y = this.userY = y ;
};


jwim.Window.prototype.close=function(){
	this.manager._destroyWindow(this);
};


jwim.Window.prototype.select=function(){
	this.manager._selectWindow(this);
};


jwim.Window.prototype.setContent=function(html,element){
	//var js, re = new RegExp('<script\b[\s\S]*?>([\s\S]*?)<\/script','gi');
	// testre doesnt need the g modifier...
	var js, testre = /<script\b[\s\S]*?>([\s\S]*?)<\/script/i;
	var afterLoad=null;
	
	if(!element){
		if( !this._unload(true) ) return false; 		
		element=this.content;
		afterLoad=this.afterLoad;
	}
	
	element.innerHTML=html.replace(/<script.*?>[\s\S]*?<\/.*?script>/gi,"");			
	this.convertElementsAttributes(element);
	
	// testre is a dirty hack to work around a FF bug
	// it seems that FF doesnt allow nested setContent calls (i mean calling setContent from a 
	// content loaded with setContent).. Infact the second (innermost) call will always get
	// the re.exec of the prv call even if html doesnt contain scripts . . mha.. 
	// The nature of this bug is not exactly clear to me.. but this works
	if(testre.test(html)){
		var re = /<script\b[\s\S]*?>([\s\S]*?)<\/script/gi;
		while (js = re.exec(html)) {
			this.scope._eval(js[1]);
		}
	}
	
	if(afterLoad)
		afterLoad(this);
	
};


jwim.Window.prototype.convertElementsAttributes=function(o){
	var evnames=['onchange','onsubmit','onreset','onselect','onblur','onfocus','onkeydown','onkeypress','onkeyup','onclick','ondblclick','onmousedown','onmousemove','onmouseout','onmouseover','onmouseup'];
	var code;
	function attachEventCode(code,thisw){
		//for IE <=7.. it may (MAY) cause conflicts if convertElementsAttributes
		// is called against a three containing objects with events attached
		if(typeof code != 'string'){
			code = code.toString();
			code=code.substr(code.indexOf('{')+1);
			code=code.substr(0,code.lastIndexOf('}'));
		}
		return function(event){
			if(!event) var event=window.event; 
			//take care of: this, event, thisw and return value..
			eval("var event_attribute=function(event,thisw){"+code+"}");
			return event_attribute.apply(this,[event,thisw]);
		}
	};	

	var elements=o.getElementsByTagName("*");
	for(var a=0; a<elements.length; a++){
		var c=elements[a];

		for(var b=0; b<evnames.length;b++){
			if(c.getAttribute && (code = c.getAttribute(evnames[b]))){					
				c[evnames[b]]=attachEventCode(code,this);
			}
		}
		
		if(c.tagName=='A' && c.target=="" &&  (code = c.getAttribute("href"))) {
			if(code.toLowerCase() != 'javascript:void(0)' && code != '#'){
				c.onclick=(function(w,c){return function(){ // since code will change we also make a copy of code 
					w.loadUrl(c);
					return false;
				}})(this,code);
			}
		}
	}

};



jwim.Window.prototype.loadUrl=function(url,onload){
	var req = false;

	//do not clear events here! if http error they must be preserverd
	if(this.onunload) 
		if( this.onunload() == false) return false;

	this.showLoading();
	// branch for native XMLHttpRequest object
	if(window.XMLHttpRequest  && !(window.ActiveXObject)) {
		try {
			req = new XMLHttpRequest();
		} catch(e) {
			req = false;
		}
	// branch for IE/Windows ActiveX version
	} else if(window.ActiveXObject) {
		try {
			req = new ActiveXObject("Msxml2.XMLHTTP");
		} catch(e) {
			try {
				req = new ActiveXObject("Microsoft.XMLHTTP");
			} catch(e) {
				req = false;
			}
		}
	}
	if(!req) {
		alert("AJAX not supported by this browser");
		return false;
	}
	req.onreadystatechange = (function(w) {return function(){
		if (req.readyState == 4) {
			w.showLoading(false);
			switch (req.status) {
				case 200: // OK
					// ensure onunlond wont be called twice
					w.onunload=null;
					w.currentUrl=url;
					w.setContent(req.responseText);	 	
					if(onload)
						onload(w);
						
					break;
				default:
					if(w.onhttperror){
						w.onhttperror(req,url);
						return;
					}
					
					if(w.manager.opts.httpErrorHandler){
						w.manager.opts.httpErrorHandler(req,url,w);
						return;
					}
					
					alert("HTTP Error: " +req.status+"\n"+url);
			}
		}
	}})(this);
	
	// cache prevention code ripped from jquery 
	if(!this.manager.opts.cacheXHR){
		var ts = (new Date).getTime();	
		// try replacing _= if it is there
		var ret = url.replace(/(\?|&)_=.*?(&|$)/, "$1_=" + ts + "$2");
		// if nothing was replaced, add timestamp to the end
		url = ret + ((ret === url) ? ( (/\?/).test(url) ? "&" : "?") + "_=" + ts : "");
	}	
	req.open("GET", url, true);
	req.send("");
	
};


jwim.Window.prototype.showLoading=function(show){
	this.loading.style.visibility = (typeof show == 'undefined' || show ? 'inherit' : 'hidden');
};


jwim.Window.prototype.setTitle=function(title){
	if(this.title){
		this.titleSpan.innerHTML=title;
		this.titleText=title;
	}
	if(this.taskbarIcon){
		this.taskbarIconSpan.innerHTML=title;
		this.taskbarIcon.setAttribute('title',title);
	}
};


jwim.Window.prototype.setMovable=function(v){ 
	this.movable=v;
};


jwim.Window.prototype.setAlwaysOnTop=function(v){
	if(v)this.select();
	this.alwaysOnTop=v;
};


jwim.Window.prototype.getComponent=function(elem){
	return this.components[elem];
};


jwim.Window.prototype.isMovable=function(){ 
	return this.movable;
};


jwim.Window.prototype.isAlwaysOnTop=function(){ 
	return this.alwaysOnTop;
};


jwim.Window.prototype.getWidth=function(){ 
	return this.w;
};


jwim.Window.prototype.getHeight=function(){
	return this.h;
};


jwim.Window.prototype.getX=function(){ 
	return this.x;
};


jwim.Window.prototype.getY=function(){ 
	return this.y;
};


jwim.Window.prototype.getId=function(){ 
	return this.id;
};


jwim.Window.prototype.getTitle=function(){
	return this.titleText;
};


jwim.Window.prototype.getCurrentUrl=function(full){ 
	// if not full remove anti-caching url variable
	return (this.manager.opts.cacheXHR || full ? this.currentUrl : this.currentUrl.replace(/(\?|&)_=.*/,'') );
};


jwim.Window.prototype.getState=function(){ 
	return this.state;
};


jwim.Window.prototype.getManager=function(){
	return this.manager;
};


jwim.Window.prototype.getContentWidth=function(){ 
	return parseInt(this.content.style.width);
};


jwim.Window.prototype.getContentHeight=function(){ 
	return parseInt(this.content.style.height);
};


jwim.Window.prototype.getContainer=function(){ 
	return this.win.parentNode;
};


jwim.Window.prototype.getContainerWidth=function(){ 
	var c =this.getContainer();
	if( c == document.body)
		return jwim.Utils.getBodySize().width;
		
	return c.clientWidth;
};


jwim.Window.prototype.getContainerHeight=function(){ 
	var c =this.getContainer();
	if( c == document.body)
		return jwim.Utils.getBodySize().height;
		
	return c.clientHeight;
};


jwim.Window.prototype.attachTo=function(el){
	el = el || this.manager.opts.container;
	el.appendChild(this.win);
};



jwim.Window.prototype._maximize=function(){
	
	if(this.state != 'maximized'){
		var w,h;
		var Dw= this.win.offsetWidth - parseInt(this.win.style.width);
		var Dh= this.win.offsetHeight - parseInt(this.win.style.height);

		
		this.win.style.top=0+'px';
		this.win.style.left=0+'px';
		this.x = this.y = 1;
		this.state='maximized'; 
		w=this.getContainerWidth()  - Dw;
		h=this.getContainerHeight() - Dh;
		this._setSize(w,h,false,'maximize');

	} else {
		this.state='window';
		//alert(this.userW);
		this._setSize(this.userW, this.userH,false,'restore');
		this._setPosition(this.userX,this.userY);
	}

};


// action is used to tell enevn handler what fired the event.. if action=='' event is NOT fired
jwim.Window.prototype._setSize=function(w,h,isResize,action){
	var off=0;
	this.win.style.height = h +'px';
	this.win.style.width = w +'px';


	var contw = w - (this.content.offsetWidth - parseInt(this.content.style.width));
	var conth = h - (this.content.offsetHeight-parseInt(this.content.style.height))  - this.manager.opts.bottomMargin - this.content.offsetTop;
	
	this.content.style.height = conth +'px';
	this.content.style.width = contw + 'px';
	if(!isResize){
		this.w=w;
		this.h=h;
	}
	
	if(action && this.onresize) 
		this.onresize(w,h,action);

}


jwim.Window.prototype.autoSize=function(){ // maybe in the future...
	this.win.style.height= 0 +'px';
	this.win.style.width=  0 +'px';
	this.content.style.height=0 +'px';
	this.content.style.width=0 + 'px';
	var w =this.content.scrollWidth+30,
		h=this.content.scrollHeight+70;
	this.setSize(w,h);
	this.state='window';

}



jwim.Window.prototype.hide=function(){ 
	this.prvState=this.state;
	this.state='hidden';
	this.win.style.visibility='hidden'; 
};


jwim.Window.prototype._iconify=function(){
	if(!this.manager.opts.taskbar || this.state=='icon')return;
	if(!this.manager.opts.alwaysShowIcons){
		this._createTaskbarIcon();	
	}
	this.taskbarIcon.className=this.manager.opts.classIcon;
	if(this.manager.selectedWindow == this) 
		this.manager.selectedWindow=null;
	this.hide();
	this.state='icon';
	var fw=this.manager._getFirstVisibleWindow();
	if(fw)fw.select();

	if(this.oniconize)
		this.oniconize();
};


jwim.Window.prototype._createTaskbarIcon=function(){
	if(!this.manager.opts.taskbar)return;
	//var width=this.manager.opts.iconWidth;
	this.taskbarIcon=document.createElement('div');
	this.taskbarIconSpan=document.createElement('span');
	this.taskbarIcon.className=this.manager.opts.classIcon;
	this.taskbarIconSpan.innerHTML=this.titleText;
	this.taskbarIcon.setAttribute('title',this.titleText);
	this.taskbarIcon.style.width=this.manager.opts.iconWidth + 'px';
	this.taskbarIcon.onclick=(function(w){ return function(e){ w.select(); }})(this);
	this.taskbarIcon.appendChild(this.taskbarIconSpan);
	this.manager.opts.taskbar.appendChild(this.taskbarIcon);
	this.components.icon=this.taskbarIcon;
	if(this.manager.opts.resizeIcons)
		this.manager._resizeIcons();

};


jwim.Window.prototype._removeTaskbarIcon=function(){
	// note that taskbaricon may be attached somewhere else at runtime
	this.taskbarIcon.parentNode.removeChild(this.taskbarIcon);
	this.taskbarIcon=null;
	if(this.manager.opts.resizeIcons) this.manager._resizeIcons();
};


//fires onunload event and clear window events
jwim.Window.prototype._unload=function(clearAll){
	if(this.onunload) 
		if( this.onunload() == false) return false;
	this.onunload = null;
	this.onselect = null;
	this.onresize=null;
	this.onmove=null;
	this.oniconize=null;
	this.ondeselect=null;
	this.onhttperror=null;
	
	if(clearAll){
		this.onclose=null;
		this.scope= new jwim.Scope(this);
	}
	
	return true;
};







/* - - UTILS - - */


jwim.Utils.getEventXY=function(e){
	if(window.event){ // prewarp
		return {x:window.event.clientX + document.body.scrollLeft + document.documentElement.scrollLeft, 
				y:window.event.clientY + document.body.scrollTop + document.documentElement.scrollTop };
		
	} else { 
		return {x:e.pageX, y:e.pageY}; 
	}
};


jwim.Utils.addEvent=function(obj, name, callback){
	if(window.addEventListener)
		obj.addEventListener(name, callback, false);
	else
		obj.attachEvent('on'+name, callback); 
};


jwim.Utils.removeEvent=function(obj, name, callback){
	if(window.removeEventListener)
		obj.removeEventListener(name, callback, false);
	else
		obj.detachEvent('on'+name, callback);
};


jwim.Utils.getBodySize=function(){
	var w = (window.innerWidth  || document.documentElement.clientWidth  || document.body.clientWidth);
	var h = (window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight);
	
	return {width:w, height:h};
};


jwim.Utils.getStyle=function(el,sp){
	if (el.currentStyle){
		// convert to camel.. ripped somewhere..
		var spc=sp.replace(/-([a-z])/ig, function(a,l) {return l.toUpperCase();});
		return el.currentStyle[sp] || el.currentStyle[spc];
	}
	else if (window.getComputedStyle)
		return document.defaultView.getComputedStyle(el,null).getPropertyValue(sp);
	return null;
};




/* - - CALLBACKS - - */

jwim.Callbacks.move = function(wm){return function(e){	
	var xy, x, y;
	if(wm.draggingWin==null)return;
	if(!e)var e = window.event;
	wm.draggingWin.state='window'; // not in dragStart,not in dragStop .. HERE!
	xy=jwim.Utils.getEventXY(e);
	wm.mouse.cur.x = xy.x;
	wm.mouse.cur.y = xy.y;
	y = (wm.mouse.cur.y - wm.mouse.click.y);
	x = (wm.mouse.cur.x - wm.mouse.click.x);
	if(wm.opts.containerBoundaries > 0){
		// these vals may be cached while moving..
		var minx=-1*(wm.draggingWin.getWidth() - wm.opts.containerBoundaries);
		var maxx=wm.draggingWin.getContainerWidth() - wm.opts.containerBoundaries;
		var maxy=wm.draggingWin.getContainerHeight() - wm.opts.containerBoundaries;
		
		if(x < minx)x = minx;
		if(x > maxx)x = maxx;		
		if(y > maxy)y = maxy;
	}
	if(wm.opts.containerBoundaries > -1){		
		if(y < 0) y = 0;
	}
	
	wm.draggingWin.win.style.top  = y + 'px';
	wm.draggingWin.win.style.left = x + 'px';

	if(wm.draggingWin.onmove) wm.draggingWin.onmove(x,y);
	
}};


jwim.Callbacks.resize = function(wm){return function(e){
	var xy,w,h;
	if(wm.draggingWin==null)return;
	wm.draggingWin.state='window'; // not in dragStart,not in dragStop .. HERE!
	if(!e)var e = window.event;
	xy=jwim.Utils.getEventXY(e);
	wm.mouse.cur.x = xy.x;
	wm.mouse.cur.y = xy.y;						
	h = (wm.draggingWin.h + (wm.mouse.cur.y - wm.mouse.click.y));		
	w = (wm.draggingWin.w + (wm.mouse.cur.x - wm.mouse.click.x));
	if(w<80)w=80;
	if(h<60)h=60;
	wm.draggingWin._setSize(w,h,true,'resize');
}};


jwim.Callbacks.dragStop=function(wm){return function(){

	if(wm.draggingWin ==  null) return;
	if(wm.draggingWin.state=='window'){ // if state=='window'  resize or move have been called
		wm.draggingWin.userW = wm.draggingWin.w = parseInt(wm.draggingWin.win.style.width);
		wm.draggingWin.userH = wm.draggingWin.h = parseInt(wm.draggingWin.win.style.height);
		
		wm.draggingWin.userX = wm.draggingWin.x = parseInt(wm.draggingWin.win.style.left);
		wm.draggingWin.userY = wm.draggingWin.y = parseInt(wm.draggingWin.win.style.top);
	}	
	jwim.Utils.removeEvent(document,'mousemove',wm.moveCallback);
	jwim.Utils.removeEvent(document,'mousemove',wm.resizeCallback);
	jwim.Utils.removeEvent(document,'mouseup',wm.dragStop);
	
	if(typeof document.onselectstart != 'undefined')
		jwim.Utils.removeEvent(document,'selectstart',jwim.Callbacks.onSelectStart);
	
	wm.draggingWin=null;

}};


jwim.Callbacks.dragStart=function(w){ return function(e){ 
	var targ, ef, xy;
	if (!e)var e = window.event;		
	
	w.select();
	
	if (e.target) targ = e.target;
	else if (e.srcElement) targ = e.srcElement;
	if (targ.nodeType == 3) // defeat Safari bug (it returns a textnode if one)
		targ = targ.parentNode;
	/*
	for(var a=0; a< w.customMovers....
		if(w.customMovers[a]== targ){targ=w.win; break}
	*/
	switch(targ){
		case w.win:
		case w.title:
		case w.titleSpan:
		case w.resizer:			
			w.manager.draggingWin=w;
			xy=jwim.Utils.getEventXY(e);
			w.manager.mouse.click.x= xy.x;
			w.manager.mouse.click.y= xy.y;
			ef = w.manager.resizeCallback;
			if(targ != w.resizer ){
				if(w.movable == false) return false;
				w.manager.mouse.click.x -= w.x;
				w.manager.mouse.click.y -= w.y;
				ef = w.manager.moveCallback;
			}
			jwim.Utils.addEvent(document,'mousemove',ef);
			jwim.Utils.addEvent(document,'mouseup',w.manager.dragStop);
			
			if(typeof document.onselectstart != 'undefined')
				jwim.Utils.addEvent(document,'selectstart',jwim.Callbacks.onSelectStart);
			
				
			return false;
	}

}};


jwim.Callbacks.onSelectStart=function(){
	return false;
};


