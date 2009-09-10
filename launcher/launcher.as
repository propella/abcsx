import com.hurlant.eval.ByteLoader;

private function read():void {
	Global.console = output;
	print("ready!\n");
   
    var loader:URLLoader = new URLLoader();
    loader.dataFormat = URLLoaderDataFormat.BINARY;
    loader.addEventListener(Event.COMPLETE, completeHandler);
    loader.addEventListener(SecurityErrorEvent.SECURITY_ERROR, securityErrorHandler);

    var request:URLRequest = new URLRequest(fileName.text);
    try {
    	loader.load(request);
    } catch (error:Error) {
	trace("Unable to load requested document.");
    }		
}

private function completeHandler(event:Event):void {
    var loader:URLLoader = URLLoader(event.target);

    print(loader.data.length + " bytes loaded.\n");
	ByteLoader.loadBytes(loader.data, true);
}

private function securityErrorHandler(event:SecurityErrorEvent):void {
    trace("securityErrorHandler: " + event);
}

