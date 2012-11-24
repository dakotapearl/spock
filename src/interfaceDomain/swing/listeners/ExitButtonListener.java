package interfaceDomain.swing.listeners;

import interfaceDomain.swing.SwingInterface;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ExitButtonListener implements ActionListener {
	
	SwingInterface swingInterface;
	
	public ExitButtonListener(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		// stop experiment
		// save network(experiment?) state
		// transfer back across network connect if necessary
		
		// Exit
		System.exit(1);
		
	}
	
}
