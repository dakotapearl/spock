package dataDomain;

import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class Datum {
	
	private int type;
	private Object value;
	private long uniqueID;
	private int energyMeasure;
	
	public Datum(int type, Object value, int energyMeasure, long uniqueID) {
		this.type = type;
		this.value = value;
		this.energyMeasure = energyMeasure;
		this.uniqueID = uniqueID; 
		
		Log.created(this.getClass());
		Log.write("Value: " + value.toString());
		
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
	
	public int getEnergyMeasure() {
		return energyMeasure;
	}
	
}
