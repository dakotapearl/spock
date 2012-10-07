package networkDomain.core;

public interface GeneFunctions {
	public Object mutate(Object originalValue, double mutationRate);
	public Object getUpperBound();
	public Object getLowerBound();
	public int compare(Object thisValue, Object otherValue); //return 0 if equal, 1 is thisValue is greater than otherValue and -1 otherwise
}
