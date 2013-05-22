package spock.dataaccess.entities;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.User;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "UserInterfaces")
public class UserInterfaceEntity implements UserInterface {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    protected String Type;
    protected String IPAddress;
    @ManyToOne
    protected UserEntity user;
    @ManyToOne
    protected ExperimentEntity experiment;

    public UserInterfaceEntity() {
    }

    public UserInterfaceEntity(String Type, String IPAddress, User user, Experiment experiment) {
        this.Type = Type;
        this.IPAddress = IPAddress;
        this.user = (UserEntity) user;
        this.experiment = (ExperimentEntity) experiment;
    }

    @Override
    public String getIPAddress() {
        return IPAddress;
    }

    @Override
    public void setIPAddress(String IPAddress) {
        this.IPAddress = IPAddress;
    }

    @Override
    public String getType() {
        return Type;
    }

    @Override
    public void setType(String Type) {
        this.Type = Type;
    }

    @Override
    public Experiment getExperiment() {
        return experiment;
    }

    @Override
    public void setExperiment(Experiment experiment) {
        this.experiment = (ExperimentEntity) experiment;
    }

    @Override
    public Long getId() {
        return id;
    }

    @Override
    public void setId(Long id) {
        this.id = id;
    }

    @Override
    public User getUser() {
        return user;
    }

    @Override
    public void setUser(User user) {
        this.user = (UserEntity) user;
    }

    
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof UserInterfaceEntity)) {
            return false;
        }
        UserInterfaceEntity other = (UserInterfaceEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.UserInterface[ id=" + id + " ]";
    }
    
}
