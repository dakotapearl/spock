package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;

import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;

public class ConsolePanel extends JPanel {
	
	private static final long serialVersionUID = -6289099717329808121L;
	public JList consolelist;
	public DefaultListModel console;
	SwingInterface swingInterface;
	
	public ConsolePanel(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
		console = new DefaultListModel();
		consolelist = new JList(console); //Not working
		//console.setPreferredSize(console.getSize().width, 2000);
	}
	
}
