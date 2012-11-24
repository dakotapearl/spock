package interfaceDomain.swing.panels;

import interfaceDomain.swing.SwingInterface;

import java.awt.BorderLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;

public class MainPanel extends JPanel {

	private static final long serialVersionUID = -8337627487623542963L;
	public JLabel statusLabel;
	SwingInterface swingInterface;
	public ExperimentPanel expPanel;
	public NetworkPanel netPanel;
	public EnvironmentPanel envPanel;
	
	public MainPanel(SwingInterface swingInterface) {
		this.swingInterface = swingInterface;
		
		setLayout(new BorderLayout());
		
		statusLabel = new JLabel("Status: Stopped");
		add(statusLabel, BorderLayout.PAGE_START);
		
		expPanel = new ExperimentPanel(swingInterface);
		netPanel = new NetworkPanel(swingInterface);
		envPanel = new EnvironmentPanel(swingInterface);
		
		add(expPanel, BorderLayout.LINE_START);
		add(netPanel, BorderLayout.CENTER);
		add(envPanel, BorderLayout.LINE_END);
	}
	
}
