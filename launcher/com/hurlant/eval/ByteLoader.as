package com.hurlant.eval {
	import flash.display.Loader;
	import flash.events.Event;
	import flash.system.ApplicationDomain;
	import flash.system.LoaderContext;
	import flash.utils.ByteArray;
	import flash.utils.Endian;
	
	public class ByteLoader {
		
		private static var swf_start:Array = 
		[
			0x46, 0x57, 0x53, 0x09, 								// FWS, Version 9
			0xff, 0xff, 0xff, 0xff, 								// File length
			0x78, 0x00, 0x03, 0xe8, 0x00, 0x00, 0x0b, 0xb8, 0x00,	// size [Rect 0 0 8000 6000] 
		 	0x00, 0x0c, 0x01, 0x00, 								// 16bit le frame rate 12, 16bit be frame count 1 
		 	0x44, 0x11,												// Tag type=69 (FileAttributes), length=4  
		 	0x08, 0x00, 0x00, 0x00
		 ];

		private static var abc_header:Array = 
		[
		 	0x3f, 0x12,												// Tag type=72 (DoABC), length=next.
		 	//0xff, 0xff, 0xff, 0xff 								// ABC length, not included in the copy. 
		];
		 
		private static var swf_end:Array =
		// the commented out code tells the player to instance a class "test" as a Sprite.
		[/*0x09, 0x13, 0x01, 0x00, 0x00, 0x00, 0x74, 0x65, 0x73, 0x74, 0x00, */ 0x40, 0x00]; // Tag type=1 (ShowFrame), length=0
		 
		
		/**
		 * Wraps the ABC bytecode inside the simplest possible SWF file, for
		 * the purpose of allowing the player VM to load it.
		 *  
		 * @param bytes: an ABC file
		 * @return a SWF file 
		 * 
		 */
		public static function wrapInSWF(bytes:Array):ByteArray {
			if (bytes.length==0) return null;
			// wrap our ABC bytecodes in a SWF.
			var out:ByteArray = new ByteArray;
			out.endian = Endian.LITTLE_ENDIAN;
			for (var i:int=0;i<swf_start.length;i++) {
				out.writeByte(swf_start[i]);
			}
			for (i=0;i<bytes.length;i++) {
				var abc:ByteArray = bytes[i];
				for (var j:int=0;j<abc_header.length;j++) {
					out.writeByte(abc_header[j]);
				}
				// set ABC length
				out.writeInt(abc.length);
				out.writeBytes(abc, 0, abc.length);
			}
			for (i=0;i<swf_end.length;i++) {
				out.writeByte(swf_end[i]);
			}
			// set SWF length
			out.position = 4;
			out.writeInt(out.length);
			// reset
			out.position = 0;
			return out;
		}
		/**
		 * Load the bytecodes passed into the flash VM, using
		 * the current application domain, or a child of it
		 *
		 * This probably always returns true, even when things fail horribly,
		 * due to the Loader logic waiting to parse the bytecodes until the 
		 * current script has finished running. 
		 * 
		 * Note: bytes can be a ByteArray, or an Array of ByteArrays.
		 * This is mostly there to allows the ESC ABCs to be loaded in one go
		 * without weird ordering issues or mysterious partial load failures. 
		 * 
		 */
		private static var loaders:Array = [];
		public static function loadBytes(bytes:*, inplace:Boolean=false, callback:Function = null):Boolean {
			if (bytes is ByteArray) bytes.position = 0;
			if (!(bytes is Array)) {
			    bytes = [ bytes ];
			}
			bytes = wrapInSWF(bytes);

			try {
				var c:LoaderContext = null;
				if (inplace) {
					c = new LoaderContext(false, ApplicationDomain.currentDomain, null);
				}
				var l:Loader = new Loader;
				if (callback!=null) {
					l.contentLoaderInfo.addEventListener(Event.INIT, callback);
				}
				l.loadBytes(bytes, c);
				loaders.push(l); // a way around poorly timed GCs.
				return true;
			} catch (e:*) {
				trace(e);
			} finally {
				//trace("done.");
				// darn it. the running of the bytes doesn't happen until current scripts are done. no try/catch can work
			}
			return false;
		}
	}
}
