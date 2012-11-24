package interfaceDomain.swing.listeners;

import interfaceDomain.swing.SwingInterface;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import tools.errorChecking.Log;
import experimentDomain.Binary.BinaryAND;

public class StartButtonListener implements ActionListener {
	
	SwingInterface swingInterface;
	
	public StartButtonListener(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		
		swingInterface.controlsPanel.startButton.setEnabled(false);
		swingInterface.controlsPanel.startButton.setText("Pause");
		
		Log.write("Interface (1): selecting experiment");
		swingInterface.exp = new BinaryAND(swingInterface.interfaceDomain.experimentDomain);
		
		Log.write("Interface (2): setting parameters");
		
		// Concurrent, no ordering after this start call can be guaranteed 
		Log.write("Interface (3): start threads");
		swingInterface.mainPanel.statusLabel.setText("Status: Started");
		swingInterface.exp.start();
	}
	
}
