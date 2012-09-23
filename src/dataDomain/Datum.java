package dataDomain;

import tools.Log;

public class Datum {
	
	private int type;
	private Object value;
	private long uniqueID;
	
	public Datum(int type, Object value) {
		this.type = type;
		this.value = value;
		
		Log.created(this.getClass());
		Log.write("Value: " + value.toString());
		
		uniqueID = DataDomain.generateUniqueIDStatic();
		
	}
	
	public long getUniqueID() {
		return uniqueID;
	}
	
	public int getType() {
		return type;
	}
	
	public Object getValue() {
		return value;
	}
	
}
