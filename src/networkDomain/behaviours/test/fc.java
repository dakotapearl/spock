package networkDomain.behaviours.test;

import networkDomain.extensions.FiringCondition;

/**
 * @author Loren Chorley
 */
public class fc extends FiringCondition {
	
	public void refresh() {
		if (parent.transmissionContent.signalsRemain())
			setReadyToFire();
		else 
			setNotReadyToFire();
	}

	@Override
	public FiringCondition replicate() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void run() {
		// TODO Auto-generated method stub
		
	}
	
}
