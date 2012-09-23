package dataDomain;

import tools.Assert;
import tools.Log;

public class DataCell {
	
	private int dimension;
	private DataCell dataCell;
	private Datum datum;
	private DataCell[][] neighbours;
	private boolean isContainer;
	
	public DataCell(int dimension, DataCell dataCell) {
		Assert.CriticalAssertTrue("dimension parameter value greater than 0", dimension > 0);
		Assert.CriticalAssertTrue("dataCell parameter not null", dataCell != null);
		this.dimension = dimension;
		this.dataCell = dataCell;
		this.isContainer = true;
		
		neighbours = new DataCell[dimension][2];
		
		Log.created(this.getClass());
	}
	
	public DataCell(int dimension, Datum datum) {
		Assert.CriticalAssertTrue("dimension value greater than 0", dimension > 0);
		Assert.CriticalAssertTrue("datum parameter not null", datum != null);
		this.dimension = dimension;
		this.datum = datum;
		this.isContainer = false;
		
		neighbours = new DataCell[dimension][2];
	}
	
	public int getDimension() {
		return dimension;
	}
	
	public DataCell getDataCell() {
		return dataCell;
	}
	
	public Datum getDatum() {
		return datum;
	}
	
	public boolean isContainer() {
		return isContainer;
	}
	
	/**
	 * @return null cell not set
	 */
	public DataCell getLowerNeighbouringDataCell(int dimension) {
		Assert.CriticalAssertTrue("dimension value within bounds", (dimension > 0) && (dimension <= this.dimension));
				
		return neighbours[0][dimension];
	}
	
	public DataCell getHigherNeighbouringDataCell(int dimension) {
		Assert.CriticalAssertTrue("dimension value within bounds", (dimension > 0) && (dimension <= this.dimension));
		
		return neighbours[1][dimension];
	}
	
	public void setLowerNeighbouringDataCell(int dimension, DataCell dataCell) {
		Assert.CriticalAssertTrue("dimension value within bounds", (dimension > 0) && (dimension <= this.dimension));
		
		neighbours[0][dimension] = dataCell;
	}
	
	public void setHigherNeighbouringDataCell(int dimension, DataCell dataCell) {
		Assert.CriticalAssertTrue("dimension value within bounds", (dimension > 0) && (dimension <= this.dimension));
		
		neighbours[1][dimension] = dataCell;
	}
	
}
