package dataDomain;

public interface DataClientInterface {

	public Datum getDatum(int type, Object value);
	public Datum getDatum(long uniqueID);
	public Long generateUniqueID();
	
}
