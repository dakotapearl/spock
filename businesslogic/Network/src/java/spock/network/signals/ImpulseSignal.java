package spock.network.signals;

import java.io.Serializable;

/**
 * @author Loren Chorley
 */
public class ImpulseSignal extends NetworkSignal {

	private Serializable data;
	
	// Needs to be able to cope with null value for networknode
	@Override
	public Serializable getData() {
		return data;
	}

	@Override
	public void setData(Serializable data) {
		this.data = data;
		
	}

}
